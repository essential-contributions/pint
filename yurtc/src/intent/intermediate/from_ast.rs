use crate::{
    ast, contract,
    error::{CompileError, Span},
    expr,
    intent::{Path, Solve},
    types,
};

use super::{
    Block, ContractDecl, Expr, FnDecl, InterfaceDecl, IntermediateIntent, State, Type, Var,
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
                    span: span.clone(),
                    msg: "Use statements must be removed from AST before conversion to Intent.",
                });
            }

            ast::Decl::Let { name, ty, init, .. } => {
                expr_ctx.check_unique_symbol(name)?;
                expr_ctx.unpack_let_decl(name, ty, init)?;
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

            ast::Decl::Fn { fn_sig, body, .. } => {
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
                    fn_sig.span.clone(),
                ))
            }

            ast::Decl::Solve { directive, span } => {
                let what = match directive {
                    ast::SolveFunc::Satisfy => Solve::Satisfy,
                    ast::SolveFunc::Minimize(id) => Solve::Minimize(convert_ident(id)?),
                    ast::SolveFunc::Maximize(id) => Solve::Maximize(convert_ident(id)?),
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
                new_types.push((name, convert_type(ty)?, span));
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
            ast::Expr::Immediate(imm) => Expr::Immediate(imm.clone()),
            ast::Expr::Path(id) => Expr::Path(convert_ident(id)?),
            ast::Expr::UnaryOp { op, expr } => Expr::UnaryOp {
                op: op.clone(),
                expr: Box::new(self.convert_expr(expr)?),
            },
            ast::Expr::BinaryOp { op, lhs, rhs } => Expr::BinaryOp {
                op: op.clone(),
                lhs: Box::new(self.convert_expr(lhs)?),
                rhs: Box::new(self.convert_expr(rhs)?),
            },
            ast::Expr::Call { name, args } => Expr::Call {
                name: convert_ident(name)?,
                args: convert_vec(args, |arg| self.convert_expr(arg))?,
            },
            ast::Expr::Block(block) => self.convert_block(block)?,
            ast::Expr::If {
                condition,
                then_block,
                else_block,
            } => Expr::If {
                condition: Box::new(self.convert_expr(condition)?),
                then_block: Block(Box::new(self.convert_block(then_block)?)),
                else_block: Block(Box::new(self.convert_block(else_block)?)),
            },
            ast::Expr::Cond {
                branches,
                else_result,
            } => Expr::Cond {
                branches: convert_vec(branches, |expr::CondBranch { condition, result }| {
                    self.convert_expr(condition).and_then(|condition| {
                        self.convert_expr(result).map(|result| expr::CondBranch {
                            condition: Box::new(condition),
                            result: Box::new(result),
                        })
                    })
                })?,
                else_result: Box::new(self.convert_expr(else_result)?),
            },
            ast::Expr::Array(els) => Expr::Array(convert_vec(els, |el| self.convert_expr(el))?),
            ast::Expr::ArrayElementAccess { array, index } => Expr::ArrayElementAccess {
                array: Box::new(self.convert_expr(array)?),
                index: Box::new(self.convert_expr(index)?),
            },
            ast::Expr::Tuple(fields) => Expr::Tuple(convert_vec(fields, |(name, expr)| {
                self.convert_expr(expr).map(|expr| (name.clone(), expr))
            })?),
            ast::Expr::TupleFieldAccess { tuple, field } => Expr::TupleFieldAccess {
                tuple: Box::new(self.convert_expr(tuple)?),
                field: field.clone(),
            },
            ast::Expr::Cast { value, ty } => Expr::Cast {
                value: Box::new(self.convert_expr(value)?),
                ty: Box::new(self.convert_type(ty)?),
            },
            ast::Expr::In { value, collection } => Expr::In {
                value: Box::new(self.convert_expr(value)?),
                collection: Box::new(self.convert_expr(collection)?),
            },
        })
    }

    fn convert_type(&mut self, ast_ty: &ast::Type) -> super::Result<Type> {
        Ok(match ast_ty {
            ast::Type::Bool => Type::Bool,
            ast::Type::Int => Type::Int,
            ast::Type::Real => Type::Real,
            ast::Type::String => Type::String,
            ast::Type::Array { ty, range } => Type::Array {
                ty: Box::new(self.convert_type(ty)?),
                range: self.convert_expr(range)?,
            },
            ast::Type::Tuple(fields) => Type::Tuple(convert_vec(fields, |(name, ast_ty)| {
                self.convert_type(ast_ty).map(|ty| (name.clone(), ty))
            })?),
            ast::Type::CustomType(name) => Type::CustomType(convert_ident(name)?),
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
        } = block;

        for statement in statements {
            match statement {
                ast::Decl::State { name, ty, init, .. } => {
                    self.check_unique_symbol(name)?;
                    self.convert_state(name, ty, init)?;
                }
                ast::Decl::Let { name, ty, init, .. } => {
                    self.check_unique_symbol(name)?;
                    self.unpack_let_decl(name, ty, init)?;
                }
                ast::Decl::NewType { name, ty, span, .. } => {
                    self.check_unique_symbol(name)?;
                    self.convert_type(ty)?;
                }
                ast::Decl::Constraint { expr, span } => {
                    let constraint = self.convert_expr(expr)?;
                    self.constraints.push((constraint, span.clone()));
                }

                // None of the following are allowed in code blocks, only the top-level scope.
                ast::Decl::Use { span, .. }
                | ast::Decl::Fn {
                    fn_sig: ast::FnSig { span, .. },
                    ..
                }
                | ast::Decl::Solve { span, .. }
                | ast::Decl::Enum(types::EnumDecl { span, .. })
                | ast::Decl::Interface(contract::InterfaceDecl { span, .. })
                | ast::Decl::Contract(contract::ContractDecl { span, .. })
                | ast::Decl::Extern { span, .. } => {
                    return Err(CompileError::Internal {
                        span: span.clone(),
                        msg: "Fn blocks may only have `let` and `constraint` decls.",
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
            interfaces: convert_vec(interfaces, convert_ident)?,
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
            };
            self.constraints.push((eq_expr, name.span.clone()));
        };

        Ok(())
    }
}

fn convert_ident(ast_id: &ast::Path) -> super::Result<Path> {
    // NOTE: for now we're only supporting a single main module, so the path MUST be a single
    // element and we're assuming is_absolute.  After we have the module system implemented then
    // all symbols will be canonicalised and an ast::Path will just be a string.
    if ast_id.path.len() != 1 {
        return Err(CompileError::Internal {
            span: ast_id.span.clone(),
            msg: "Multi-path identifiers are not supported yet.",
        });
    }
    Ok(ast_id.path[0].name.clone())
}

// We're converting vectors by mapping other conversions over their elements.  This little utility
// makes it easy and type-checked.
fn convert_vec<E, R, F>(vec: &[E], f: F) -> super::Result<Vec<R>>
where
    F: FnMut(&E) -> super::Result<R>,
{
    vec.iter().map(f).collect()
}
