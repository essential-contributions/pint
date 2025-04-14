#![allow(dead_code)]
#![allow(unused_variables)]

mod display;

// XXX: There are XXXs which need looking at.

use crate::{
    error::{ErrorEmitted, Handler},
    expr::{BinaryOp, Expr, Immediate, UnaryOp},
    predicate,
    span::empty_span,
};

use essential_asm::{short::*, Op as AsmOp};

pub fn compile_contract(
    handler: &Handler,
    contract: &predicate::Contract,
) -> Result<Contract, ErrorEmitted> {
    let preds = contract
        .preds
        .iter()
        .map(|(key, pred)| compile_pred(handler, contract, pred))
        .collect::<Result<_, _>>()?;

    Ok(Contract { preds })
}

pub struct Contract {
    preds: Vec<Predicate>,
}

fn compile_pred(
    handler: &Handler,
    contract: &predicate::Contract,
    predicate: &predicate::Predicate,
) -> Result<Predicate, ErrorEmitted> {
    // Iterate for each constraint and build IR values.
    let mut compiler = PredicateCompiler::new(handler, contract, predicate);

    // XXX: Need to compile `let` variables and store them in the symbol table first.

    for constraint_decl in &predicate.constraints {
        compiler.compile_constraint(constraint_decl.expr)?;
    }

    Ok(compiler.finish())
}

slotmap::new_key_type! { pub struct NodeKey; }

pub struct Predicate {
    nodes: slotmap::SlotMap<NodeKey, Node>,
    constraints: Vec<NodeKey>,
}

struct Node {
    value: Value,
    size: usize,
    loc: Location,
}

#[derive(Clone, Copy)]
enum Location {
    Stack,
    Memory,
    ParamData,
    Storage, // Needed?
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Location::Stack => write!(f, "stack"),
            Location::Memory => write!(f, "mem"),
            Location::ParamData => write!(f, "param"),
            Location::Storage => write!(f, "storage"),
        }
    }
}

enum Value {
    Data(Vec<i64>),

    Operation(AsmOp),
    OperationBlock(Vec<AsmOp>),

    Application {
        op: NodeKey,
        args: Vec<NodeKey>,
    },

    Ptr(i64),

    Gep {
        ptr: NodeKey,
        offs: i64,
    },

    MemCpy(NodeKey),
    MemMove(NodeKey),

    If {
        pred: NodeKey,
        then_val: NodeKey,
        else_val: NodeKey,
    },

    Loop {
        count: NodeKey,
        value: NodeKey,
    },
}

struct PredicateCompiler<'a> {
    handler: &'a Handler,
    contract: &'a predicate::Contract,
    predicate: &'a predicate::Predicate,

    nodes: slotmap::SlotMap<NodeKey, Node>,
    constraints: Vec<NodeKey>,

    symbols: fxhash::FxHashMap<String, NodeKey>,
}

impl<'a> PredicateCompiler<'a> {
    fn new(
        handler: &'a Handler,
        contract: &'a predicate::Contract,
        predicate: &'a predicate::Predicate,
    ) -> Self {
        Self {
            handler,
            contract,
            predicate,
            nodes: slotmap::SlotMap::default(),
            constraints: Vec::default(),
            symbols: fxhash::FxHashMap::default(),
        }
    }

    fn finish(self) -> Predicate {
        Predicate {
            nodes: self.nodes,
            constraints: self.constraints,
        }
    }

    fn compile_constraint(&mut self, expr_key: predicate::ExprKey) -> Result<(), ErrorEmitted> {
        let expr_node_key = self.compile_expr_key(expr_key)?;

        self.constraints.push(expr_node_key);

        Ok(())
    }

    fn compile_expr_key(&mut self, expr_key: predicate::ExprKey) -> Result<NodeKey, ErrorEmitted> {
        match expr_key.get(&self.contract) {
            Expr::Immediate { value, .. } => self.compile_immediate(value),

            Expr::Array {
                elements,
                range_expr,
                span,
            } => todo!(),

            Expr::Tuple { fields, span } => todo!(),

            Expr::UnionVariant {
                path,
                path_span,
                value,
                span,
            } => todo!(),

            Expr::Nil(span) => todo!(),

            Expr::KeyValue { lhs, rhs, span } => todo!(),

            Expr::Path(path, _) => self.compile_path(path),

            Expr::AsmBlock { args, ops, span } => todo!(),

            Expr::UnaryOp { op, expr, span } => match op {
                UnaryOp::Neg => self.compile_op_neg(expr),
                UnaryOp::Not => self.compile_operator(NOT, expr, None),

                UnaryOp::Unwrap => todo!(),

                UnaryOp::NextState | UnaryOp::Error => Err(self
                    .handler
                    .emit_internal_err(format!("unexpected operator: {op:?}"), span.clone())),
            },

            Expr::BinaryOp { op, lhs, rhs, span } => {
                if let Some(asm_op) = Self::translate_op(op) {
                    self.compile_operator(asm_op, lhs, Some(rhs))
                } else {
                    match op {
                        BinaryOp::NotEqual => self.compile_op_neq(lhs, rhs),

                        BinaryOp::LogicalAnd => todo!(),
                        BinaryOp::LogicalOr => todo!(),

                        BinaryOp::Concat => todo!(),

                        _ => unreachable!(),
                    }
                }
            }

            Expr::IntrinsicCall { kind, args, span } => todo!(),

            Expr::LocalPredicateCall {
                predicate,
                args,
                span,
            } => todo!(),

            Expr::ExternalPredicateCall {
                interface,
                c_addr,
                predicate,
                p_addr,
                args,
                span,
            } => todo!(),

            Expr::Select {
                condition,
                then_expr,
                else_expr,
                span,
            } => todo!(),

            Expr::Index { expr, index, span } => todo!(),

            Expr::TupleFieldAccess { tuple, field, span } => todo!(),

            Expr::Map {
                param,
                range,
                body,
                span,
            } => todo!(),

            Expr::UnionTag { union_expr, span } => todo!(),

            Expr::UnionValue {
                union_expr,
                variant_ty,
                span,
            } => todo!(),

            Expr::Error(_)
            | Expr::LocalStorageAccess { .. }
            | Expr::ExternalStorageAccess { .. }
            | Expr::MacroCall { .. }
            | Expr::Cast { .. }
            | Expr::In { .. }
            | Expr::Range { .. }
            | Expr::Generator { .. }
            | Expr::Match { .. } => Err(self.handler.emit_internal_err(
                "These expressions should have been lowered by now",
                empty_span(),
            )),
        }
    }

    fn compile_immediate(&mut self, imm: &Immediate) -> Result<NodeKey, ErrorEmitted> {
        fn compile_imm_to_data(
            handler: &Handler,
            imm: &Immediate,
        ) -> Result<Vec<i64>, ErrorEmitted> {
            match imm {
                Immediate::Int(n) => Ok(vec![*n]),
                Immediate::Bool(b) => Ok(vec![*b as i64]),

                Immediate::B256(ns) => Ok(ns.iter().map(|n| *n as i64).collect()),

                Immediate::Array(els) => els
                    .iter()
                    .map(|el| compile_imm_to_data(handler, el))
                    .collect::<Result<Vec<_>, _>>()
                    .map(|data| data.into_iter().flatten().collect()),

                Immediate::Tuple(els) => els
                    .iter()
                    .map(|(_, el)| compile_imm_to_data(handler, el))
                    .collect::<Result<Vec<_>, _>>()
                    .map(|data| data.into_iter().flatten().collect()),

                Immediate::UnionVariant {
                    tag_num,
                    value_size,
                    value,
                    decl,
                } => {
                    let mut data = Vec::default();

                    data.push(*tag_num);

                    if let Some(value) = value {
                        data.append(&mut compile_imm_to_data(handler, value)?);
                    }

                    while data.len() < *value_size {
                        data.push(0);
                    }

                    Ok(data)
                }

                Immediate::Error | Immediate::Real(_) | Immediate::String(_) => {
                    Err(handler.emit_internal_err("unexpected literal", empty_span()))
                }
            }
        }

        let data = compile_imm_to_data(self.handler, imm)?;
        let size = data.len();

        // All immediates compile to the stack.
        Ok(self.nodes.insert(Node {
            value: Value::Data(data),
            size,
            loc: Location::Stack,
        }))
    }

    fn translate_op(op: &BinaryOp) -> Option<AsmOp> {
        match op {
            BinaryOp::Add => Some(ADD),
            BinaryOp::Sub => Some(SUB),
            BinaryOp::Mul => Some(MUL),
            BinaryOp::Div => Some(DIV),
            BinaryOp::Mod => Some(MOD),
            BinaryOp::Equal => Some(EQ),
            BinaryOp::LessThanOrEqual => Some(LTE),
            BinaryOp::LessThan => Some(LT),
            BinaryOp::GreaterThanOrEqual => Some(GTE),
            BinaryOp::GreaterThan => Some(GT),

            BinaryOp::NotEqual | BinaryOp::LogicalAnd | BinaryOp::LogicalOr | BinaryOp::Concat => {
                None
            }
        }
    }

    fn compile_operator(
        &mut self,
        op: AsmOp,
        arg1: &predicate::ExprKey,
        arg2: Option<&predicate::ExprKey>,
    ) -> Result<NodeKey, ErrorEmitted> {
        let mut app_args = Vec::default();

        let arg1_node_key = self.compile_expr_key(*arg1)?;
        app_args.push(self.move_node_to(arg1_node_key, 1, Location::Stack));

        if let Some(arg2_expr_key) = arg2 {
            let arg2_node_key = self.compile_expr_key(*arg2_expr_key)?;
            app_args.push(self.move_node_to(arg2_node_key, 1, Location::Stack));
        }

        let applicand_node_key = self.nodes.insert(Node {
            value: Value::Operation(op),
            size: 1,
            loc: Location::Stack,
        });

        Ok(self.apply_node_to(applicand_node_key, app_args))
    }

    fn compile_op_neg(&mut self, expr: &predicate::ExprKey) -> Result<NodeKey, ErrorEmitted> {
        todo!();
        // let zero_node = compile_immediate(nodes, symbols, handler, contract, &Immediate::Int(0))?;
        // let val_node = compile_expr_key(nodes, symbols, handler, contract, *expr)?;
        // let sub_node = nodes.insert(Node {
        //     value: Value::Operation(SUB),
        //     users: Vec::default(),
        // });
        // compile_app(nodes, handler, sub_node, vec![zero_node, val_node])
    }

    fn compile_op_neq(
        &mut self,
        lhs: &predicate::ExprKey,
        rhs: &predicate::ExprKey,
    ) -> Result<NodeKey, ErrorEmitted> {
        todo!();

        // let eq_node = compile_op(nodes, symbols, handler, contract, EQ, lhs, Some(rhs))?;
        // let not_node = nodes.insert(Node {
        //     value: todo!(),
        //     users: todo!(),
        // });
        //
        // compile_app(nodes, handler, not_node, vec![eq_node])
    }

    fn compile_path(&mut self, path: &str) -> Result<NodeKey, ErrorEmitted> {
        if let Some(existing_node_key) = self.symbols.get(path) {
            return Ok(*existing_node_key);
        }

        // Could be:
        // - morphism param (in symbols already)
        // - pred param (self.predicate.params)
        // - let variables (self.predicate.variables)

        if let Some((param_idx, _)) = self
            .predicate
            .params
            .iter()
            .enumerate()
            .find(|(_, param)| &param.name.name == path)
        {
            Ok(self.nodes.insert(Node {
                value: Value::Ptr(param_idx as i64),
                size: 0,
                loc: Location::ParamData,
            }))
        } else {
            todo!()
        }
    }

    fn apply_node_to(&mut self, applicand: NodeKey, args: Vec<NodeKey>) -> NodeKey {
        let appand = &self.nodes[applicand];

        self.nodes.insert(Node {
            value: Value::Application { op: applicand, args },
            size: appand.size,
            loc: appand.loc,
        })
    }

    fn move_node_to(&mut self, src_node_key: NodeKey, size: usize, loc: Location) -> NodeKey {
        self.nodes.insert(Node {
            value: Value::MemMove(src_node_key),
            size,
            loc,
        })
    }
}
