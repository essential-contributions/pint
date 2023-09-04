use crate::{
    ast::{self, Ident},
    contract,
    error::CompileError,
    expr,
    intent::Path,
    span::Span,
    types,
};

use super::{
    Block, ContractDecl, Expr, FnDecl, InterfaceDecl, IntermediateIntent, SolveFunc, State, Type,
    Var,
};

use std::collections::HashMap;

pub(super) fn from_ast(ast: &[ast::Decl]) -> super::Result<IntermediateIntent> {
    let mut expr_ctx = ExprContext::default();

    let mut directives = Vec::new();

    let mut funcs = Vec::new();
    let mut enums = Vec::new();
    let mut interfaces = Vec::new();
    let mut contracts = Vec::new();
    let mut externs = Vec::new();
    let mut new_types = Vec::new();

    for decl in ast {
        match decl {
            ast::Decl::Use { span, .. } => {
                return Err(CompileError::Internal {
                    msg: "Use statements must be removed from AST before conversion to Intent.",
                    span: span.clone(),
                });
            }

            ast::Decl::Let {
                name,
                ty,
                init,
                span,
            } => {
                expr_ctx.check_unique_symbol(name)?;
                expr_ctx.unpack_let_decl(name, ty, init, span)?;
            }

            ast::Decl::State { name, ty, init, .. } => {
                expr_ctx.check_unique_symbol(name)?;
                expr_ctx.convert_state(name, ty, init)?;
            }

            ast::Decl::Enum(enum_decl) => {
                enums.push(enum_decl.clone());
            }

            ast::Decl::Constraint { expr, span } => {
                let expr = expr_ctx.convert_expr(expr)?;
                expr_ctx.constraints.push((expr, span.clone()));
            }

            ast::Decl::Fn { fn_sig, body, span } => {
                expr_ctx.check_unique_symbol(&fn_sig.name)?;

                let mut local_expr_ctx = ExprContext::default();
                let returned_constraint = local_expr_ctx.convert_block(body)?;

                funcs.push((
                    FnDecl {
                        sig: expr_ctx.convert_fn_sig(fn_sig)?,
                        local_vars: local_expr_ctx.vars,
                        local_constraints: local_expr_ctx.constraints,
                        returned_constraint,
                    },
                    span.clone(),
                ))
            }

            ast::Decl::Solve { directive, span } => {
                let what = match directive {
                    ast::SolveFunc::Satisfy => SolveFunc::Satisfy,
                    ast::SolveFunc::Minimize(id) => SolveFunc::Minimize(expr_ctx.convert_expr(id)?),
                    ast::SolveFunc::Maximize(id) => SolveFunc::Maximize(expr_ctx.convert_expr(id)?),
                };
                directives.push((what, span.clone()));
            }

            ast::Decl::Interface(iface_decl) => {
                interfaces.push(expr_ctx.convert_interface(iface_decl)?);
            }

            ast::Decl::Contract(contract_decl) => {
                contracts.push(expr_ctx.convert_contract(contract_decl)?);
            }
            ast::Decl::Extern { functions, span } => {
                externs.push((
                    convert_vec(functions, |fnsig| expr_ctx.convert_fn_sig(fnsig))?,
                    span.clone(),
                ));
            }
            ast::Decl::NewType { name, ty, span } => {
                expr_ctx.check_unique_symbol(name)?;
                new_types.push((name, expr_ctx.convert_type(ty)?, span));
            }
        }
    }

    Ok(IntermediateIntent {
        states: expr_ctx.states,
        vars: expr_ctx.vars,
        constraints: expr_ctx.constraints,
        directives,
        funcs,
        enums,
        interfaces,
        contracts,
        externs,
    })
}

#[derive(Default)]
struct ExprContext {
    names: HashMap<Path, Span>,

    states: Vec<(State, Span)>,
    vars: Vec<(Var, Span)>,
    constraints: Vec<(Expr, Span)>,
    new_types: Vec<(Ident, Type, Span)>,
}

impl ExprContext {
    fn check_unique_symbol(&mut self, sym: &ast::Ident) -> super::Result<()> {
        self.names
            .get(&sym.name)
            .map(|prev_span| {
                Err(CompileError::NameClash {
                    sym: sym.name.clone(),
                    span: sym.span.clone(),
                    prev_span: prev_span.clone(),
                })
            })
            .unwrap_or_else(|| {
                self.names.insert(sym.name.clone(), sym.span.clone());
                Ok(())
            })
    }

    fn convert_expr(&mut self, ast_expr: &ast::Expr) -> super::Result<Expr> {
        Ok(match ast_expr {
            ast::Expr::Immediate { value, span } => Expr::Immediate {
                value: value.clone(),
                span: span.clone(),
            },
            ast::Expr::Path(path) => Expr::Path(convert_path(path)?),
            ast::Expr::UnaryOp { op, expr, span } => Expr::UnaryOp {
                op: op.clone(),
                expr: Box::new(self.convert_expr(expr)?),
                span: span.clone(),
            },
            ast::Expr::BinaryOp { op, lhs, rhs, span } => Expr::BinaryOp {
                op: op.clone(),
                lhs: Box::new(self.convert_expr(lhs)?),
                rhs: Box::new(self.convert_expr(rhs)?),
                span: span.clone(),
            },
            ast::Expr::Call { name, args, span } => Expr::Call {
                name: convert_path(name)?,
                args: convert_vec(args, |arg| self.convert_expr(arg))?,
                span: span.clone(),
            },
            ast::Expr::Block(block) => self.convert_block(block)?,
            ast::Expr::If {
                condition,
                then_block,
                else_block,
                span,
            } => Expr::If {
                condition: Box::new(self.convert_expr(condition)?),
                then_block: Block(Box::new(self.convert_block(then_block)?)),
                else_block: Block(Box::new(self.convert_block(else_block)?)),
                span: span.clone(),
            },
            ast::Expr::Cond {
                branches,
                else_result,
                span,
            } => Expr::Cond {
                branches: convert_vec(
                    branches,
                    |expr::CondBranch {
                         condition,
                         result,
                         span,
                     }| {
                        self.convert_expr(condition).and_then(|condition| {
                            self.convert_expr(result).map(|result| expr::CondBranch {
                                condition: Box::new(condition),
                                result: Box::new(result),
                                span: span.clone(),
                            })
                        })
                    },
                )?,
                else_result: Box::new(self.convert_expr(else_result)?),
                span: span.clone(),
            },
            ast::Expr::Array { elements, span } => Expr::Array {
                elements: convert_vec(elements, |element| self.convert_expr(element))?,
                span: span.clone(),
            },
            ast::Expr::ArrayElementAccess { array, index, span } => Expr::ArrayElementAccess {
                array: Box::new(self.convert_expr(array)?),
                index: Box::new(self.convert_expr(index)?),
                span: span.clone(),
            },
            ast::Expr::Tuple { fields, span } => Expr::Tuple {
                fields: convert_vec(fields, |(name, expr)| {
                    self.convert_expr(expr).map(|expr| (name.clone(), expr))
                })?,
                span: span.clone(),
            },
            ast::Expr::TupleFieldAccess { tuple, field, span } => Expr::TupleFieldAccess {
                tuple: Box::new(self.convert_expr(tuple)?),
                field: field.clone(),
                span: span.clone(),
            },
            ast::Expr::Cast { value, ty, span } => Expr::Cast {
                value: Box::new(self.convert_expr(value)?),
                ty: Box::new(self.convert_type(ty)?),
                span: span.clone(),
            },
            ast::Expr::In {
                value,
                collection,
                span,
            } => Expr::In {
                value: Box::new(self.convert_expr(value)?),
                collection: Box::new(self.convert_expr(collection)?),
                span: span.clone(),
            },
        })
    }

    fn convert_type(&mut self, ast_ty: &ast::Type) -> super::Result<Type> {
        Ok(match ast_ty {
            ast::Type::Primitive { kind, span } => Type::Primitive {
                kind: kind.clone(),
                span: span.clone(),
            },
            ast::Type::Array { ty, range, span } => Type::Array {
                ty: Box::new(self.convert_type(ty)?),
                range: self.convert_expr(range)?,
                span: span.clone(),
            },
            ast::Type::Tuple { fields, span } => Type::Tuple {
                fields: convert_vec(fields, |(name, ast_ty)| {
                    self.convert_type(ast_ty).map(|ty| (name.clone(), ty))
                })?,
                span: span.clone(),
            },
            ast::Type::CustomType { path, span } => Type::CustomType {
                path: convert_path(path)?,
                span: span.clone(),
            },
        })
    }

    fn convert_fn_sig(&mut self, ast_fn_sig: &ast::FnSig) -> super::Result<types::FnSig<Type>> {
        let ast::FnSig {
            name,
            params,
            return_type,
            span,
        } = ast_fn_sig;

        Ok(types::FnSig {
            name: name.clone(),
            params: convert_vec(params, |(name, ast_ty)| {
                self.convert_type(ast_ty).map(|ty| (name.clone(), ty))
            })?,
            return_type: self.convert_type(return_type)?,
            span: span.clone(),
        })
    }

    fn convert_block(&mut self, block: &ast::Block) -> super::Result<Expr> {
        let ast::Block {
            statements,
            final_expr,
            ..
        } = block;

        for statement in statements {
            match statement {
                ast::Decl::State { name, ty, init, .. } => {
                    self.check_unique_symbol(name)?;
                    self.convert_state(name, ty, init)?;
                }
                ast::Decl::Let {
                    name,
                    ty,
                    init,
                    span,
                } => {
                    self.check_unique_symbol(name)?;
                    self.unpack_let_decl(name, ty, init, span)?;
                }
                ast::Decl::NewType { name, ty, span, .. } => {
                    self.check_unique_symbol(name)?;
                    let converted_type = self.convert_type(ty)?;
                    self.new_types
                        .push((name.clone(), converted_type, span.clone()));
                }

                ast::Decl::Constraint { expr, span } => {
                    let constraint = self.convert_expr(expr)?;
                    self.constraints.push((constraint, span.clone()));
                }

                // None of the following are allowed in code blocks, only the top-level scope.
                ast::Decl::Use { span, .. }
                | ast::Decl::Fn { span, .. }
                | ast::Decl::Solve { span, .. }
                | ast::Decl::Enum(types::EnumDecl { span, .. })
                | ast::Decl::Interface(contract::InterfaceDecl { span, .. })
                | ast::Decl::Contract(contract::ContractDecl { span, .. })
                | ast::Decl::Extern { span, .. } => {
                    return Err(CompileError::Internal {
                        msg: "Fn blocks may only have `let` and `constraint` decls.",
                        span: span.clone(),
                    });
                }
            }
        }

        self.convert_expr(final_expr)
    }

    fn convert_interface(
        &mut self,
        ast_iface: &ast::InterfaceDecl,
    ) -> super::Result<InterfaceDecl> {
        let ast::InterfaceDecl {
            name,
            functions,
            span,
        } = ast_iface;

        Ok(InterfaceDecl {
            name: name.clone(),
            functions: convert_vec(functions, |sig| self.convert_fn_sig(sig))?,
            span: span.clone(),
        })
    }

    fn convert_contract(
        &mut self,
        ast_contract: &ast::ContractDecl,
    ) -> super::Result<ContractDecl> {
        let ast::ContractDecl {
            name,
            id,
            interfaces,
            functions,
            span,
        } = ast_contract;

        Ok(ContractDecl {
            name: name.clone(),
            id: self.convert_expr(id)?,
            interfaces: convert_vec(interfaces, convert_path)?,
            functions: convert_vec(functions, |sig| self.convert_fn_sig(sig))?,
            span: span.clone(),
        })
    }

    fn convert_state(
        &mut self,
        name: &ast::Ident,
        ty: &Option<ast::Type>,
        init: &ast::Expr,
    ) -> super::Result<()> {
        let ty = ty.as_ref().map(|ty| self.convert_type(ty)).transpose()?;
        let expr = self.convert_expr(init)?;
        self.states.push((
            State {
                name: name.name.to_owned(),
                ty,
                expr,
            },
            name.span.clone(),
        ));

        Ok(())
    }

    fn unpack_let_decl(
        &mut self,
        name: &ast::Ident,
        ty: &Option<ast::Type>,
        init: &Option<ast::Expr>,
        span: &Span,
    ) -> super::Result<()> {
        let ty = ty.as_ref().map(|ty| self.convert_type(ty)).transpose()?;
        self.vars.push((
            Var {
                name: name.name.to_owned(),
                ty,
            },
            name.span.clone(),
        ));

        if let Some(init) = init {
            let eq_expr = Expr::BinaryOp {
                op: expr::BinaryOp::Equal,
                lhs: Box::new(Expr::Path(name.name.to_owned())),
                rhs: Box::new(self.convert_expr(init)?),
                span: span.clone(), // Using the span of the `let` decl here
            };
            self.constraints.push((eq_expr, name.span.clone()));
        };

        Ok(())
    }
}

fn convert_path(path: &ast::Path) -> super::Result<Path> {
    // NOTE: for now we're only supporting a single main module, so the path MUST be a single
    // element and we're assuming is_absolute.  After we have the module system implemented then
    // all symbols will be canonicalised and an ast::Path will just be a string.
    if path.path.len() != 1 {
        return Err(CompileError::Internal {
            msg: "Multi-path identifiers are not supported yet.",
            span: path.span.clone(),
        });
    }
    Ok(path.path[0].name.clone())
}

// We're converting vectors by mapping other conversions over their elements.  This little utility
// makes it easy and type-checked.
fn convert_vec<E, R, F>(vec: &[E], f: F) -> super::Result<Vec<R>>
where
    F: FnMut(&E) -> super::Result<R>,
{
    vec.iter().map(f).collect()
}
