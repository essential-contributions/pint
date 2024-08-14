use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{BinaryOp, Expr, Immediate, TupleAccess, UnaryOp},
    predicate::{ConstraintDecl, Contract, ExprKey, Predicate, State as StateVar},
    span::empty_span,
    types::Type,
};
use essential_types::{
    predicate::{Directive, Predicate as CompiledPredicate},
    ContentAddress,
};
use petgraph::{graph::NodeIndex, Graph};
use state_asm::{
    Access, Alu, Constraint, Crypto, Op as StateRead, Pred, Stack, StateSlots, TotalControlFlow,
};
use std::collections::{BTreeMap, HashMap};

mod display;
#[cfg(test)]
mod tests;

#[derive(Debug, Default, Clone)]
pub struct CompiledContract {
    pub names: Vec<String>,
    pub salt: [u8; 32],
    pub predicates: Vec<CompiledPredicate>,
}

/// Convert a `Contract` into `CompiledContract`
pub fn compile_contract(
    handler: &Handler,
    contract: &Contract,
) -> Result<CompiledContract, ErrorEmitted> {
    let mut predicate_names = Vec::new();
    let mut compiled_predicates = Vec::new();

    // This is a dependnecy graph between predicates. Predicates may depend on other predicates via
    // predicate instances that reference other predicates in the same contract
    let mut dep_graph = Graph::<String, ()>::new();

    // This map keeps track of what node indices (in the dependency graph) are assigned to what
    // predicates (identified by names)
    let mut dep_graph_indices = HashMap::<String, NodeIndex>::new();

    // This is a map between node indices in the dependency graph and references to the
    // corresponding predicates.
    let mut predicates = HashMap::<NodeIndex, &Predicate>::new();

    for (_, pred) in contract.preds.iter() {
        let new_node = dep_graph.add_node(pred.name.clone());
        dep_graph_indices.insert(pred.name.clone(), new_node);
        predicates.insert(new_node, pred);
    }

    for (pred_key, pred) in contract.preds.iter() {
        // If this predicate references another predicate (via a `__sibling_predicate_address`
        // intrinsic), then create a dependency edge from the other pedicate to this one.
        for expr in contract.exprs(pred_key) {
            if let Some(Expr::IntrinsicCall { name, args, .. }) = expr.try_get(contract) {
                if name.name == "__sibling_predicate_address" {
                    if let Some(name) = args.first() {
                        if let Some(Expr::Immediate {
                            value: Immediate::String(s),
                            ..
                        }) = name.try_get(contract)
                        {
                            let from = dep_graph_indices[s];
                            let to = dep_graph_indices[&pred.name];
                            dep_graph.add_edge(from, to, ());
                        }
                    }
                }
            }
        }
    }

    // Predicates should be sorted topologically based on the dependency graph. That way, predicate
    // addresses that are required by other predicates are known in time.
    let Ok(sorted_predicates) = petgraph::algo::toposort(&dep_graph, None) else {
        return Err(handler.emit_err(Error::Compile {
            error: CompileError::Internal {
                msg: "dependency cycles between predicates should have been caught before",
                span: empty_span(),
            },
        }));
    };

    // Now compile all predicates in topological order
    let mut predicate_addresses: BTreeMap<String, ContentAddress> = Default::default();
    for idx in &sorted_predicates {
        let pred = predicates[idx];

        if let Ok(compiled_predicate) =
            handler.scope(|handler| predicate_to_asm(handler, contract, &predicate_addresses, pred))
        {
            predicate_names.push(pred.name.clone());
            predicate_addresses.insert(
                pred.name.clone(),
                essential_hash::content_addr(&compiled_predicate),
            );
            compiled_predicates.push(compiled_predicate);
        }
    }

    if handler.has_errors() {
        Err(handler.cancel())
    } else {
        Ok(CompiledContract {
            names: predicate_names,
            // Salt is not used by pint yet.
            salt: Default::default(),
            predicates: compiled_predicates,
        })
    }
}

pub struct AsmBuilder<'a> {
    // Opcodes to read state.
    s_asm: Vec<Vec<StateRead>>,
    // Opcodes to specify constraints
    c_asm: Vec<Vec<Constraint>>,
    // A reference to a list of the predicate addresses which are known so far
    predicate_addresses: &'a BTreeMap<String, ContentAddress>,
}

#[derive(Debug)]
struct StorageKey {
    len: usize,
    is_extern: bool,
}

/// Location of an expression:
/// 1. `Value` expressions are just raw values such as immediates or the outputs of binary ops.
/// 2. `DecisionVar` expressions refer to expressions that require the `DecisionVar` or
///    `DecisionVarRange` opcodes. The `usize` here is the index of the decision variable.
/// 3. `Transient` expressions refer to expressions that require the `Transient` opcode. The first
///    `ExprKey` is an expression that represents the transient key length. The second `ExprKey` is
///    an expression that represents the pathway.
/// 4. `State` expressions refer to expressions that require the `State` or `StateRange` opcodes.
///    The `bool` is the "delta": are we referring to the current or the next state?
enum Location {
    Value,
    DecisionVar(usize),
    Transient { key_len: ExprKey, pathway: ExprKey },
    State(bool),
}

impl AsmBuilder<'_> {
    /// Given an `expr`, compile and calculate its `Location`. Only a "pointer" is produced or
    /// nothing if the expression is a raw value.
    fn compile_expr_pointer(
        &self,
        handler: &Handler,
        asm: &mut Vec<Constraint>,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        match &expr.get(contract) {
            // All of these are just values
            Expr::Error(_)
            | Expr::Immediate { .. }
            | Expr::Array { .. }
            | Expr::Tuple { .. }
            | Expr::BinaryOp { .. }
            | Expr::MacroCall { .. }
            | Expr::Select { .. }
            | Expr::Cast { .. }
            | Expr::In { .. }
            | Expr::Range { .. }
            | Expr::Generator { .. }
            | Expr::StorageAccess { .. }
            | Expr::ExternalStorageAccess { .. } => Ok(Location::Value),
            Expr::IntrinsicCall { name, args, .. } => {
                if name.name == "__transient" {
                    // This particular intrinsic returns a `Location::Transient`. All other
                    // intrinsics are just values.

                    // Expecting 3 arguments:
                    assert_eq!(args.len(), 3);
                    // First argument is a key. Let that be anything

                    // Second argument is a key length. We want that to be an integer
                    assert!(args[1].get_ty(contract).is_int());

                    // Third argument is the "pathway". We want that to be a path expression (to a
                    // pathway variable) or an intrinsic call (to `__this_pathway`). Not being too
                    // exact here but that's fine since this is all internal anyways.
                    assert!(matches!(
                        args[2].try_get(contract),
                        Some(Expr::Path(..) | Expr::IntrinsicCall { .. })
                    ));

                    // First, compile the key
                    self.compile_expr(handler, asm, &args[0], contract, pred)?;

                    Ok(Location::Transient {
                        key_len: args[1],
                        pathway: args[2],
                    })
                } else {
                    Ok(Location::Value)
                }
            }
            Expr::UnaryOp { op, expr, .. } => match op {
                UnaryOp::NextState => {
                    // Next state expressions produce state expressions (i.e. ones that require
                    // `State` or `StateRange`
                    self.compile_expr_pointer(handler, asm, expr, contract, pred)?;
                    Ok(Location::State(true))
                }
                _ => Ok(Location::Value),
            },
            Expr::Path(path, _) => Self::compile_path(handler, asm, path, contract, pred),
            Expr::TupleFieldAccess { tuple, field, .. } => {
                let location = self.compile_expr_pointer(handler, asm, tuple, contract, pred)?;
                match location {
                    Location::State(_) | Location::Transient { .. } | Location::DecisionVar(_) => {
                        // Offset calculation is pretty much the same for all types of data

                        // Grab the fields of the tuple
                        let Type::Tuple { ref fields, .. } = tuple.get_ty(contract) else {
                            return Err(handler.emit_err(Error::Compile {
                                error: CompileError::Internal {
                                    msg: "type must exist and be a tuple type",
                                    span: empty_span(),
                                },
                            }));
                        };

                        // The field index is based on the type definition
                        let field_idx = match field {
                            TupleAccess::Index(idx) => *idx,
                            TupleAccess::Name(ident) => fields
                                .iter()
                                .position(|(field_name, _)| {
                                    field_name
                                        .as_ref()
                                        .map_or(false, |name| name.name == ident.name)
                                })
                                .expect("field name must exist, this was checked in type checking"),
                            TupleAccess::Error => {
                                return Err(handler.emit_err(Error::Compile {
                                    error: CompileError::Internal {
                                        msg: "unexpected TupleAccess::Error",
                                        span: empty_span(),
                                    },
                                }));
                            }
                        };

                        // This is the offset from the base key where the full tuple is stored.
                        let key_offset: usize =
                            fields.iter().take(field_idx).try_fold(0, |acc, (_, ty)| {
                                // Decision vars are flattened in a given slots, so we look at the
                                // raw size in words. For transient and storage variables, we look
                                // at the "storage size" which may yield different results (e.g. a
                                // `b256` is 4 words but its storage size is 1
                                if let Location::DecisionVar(_) = location {
                                    ty.size(handler, contract)
                                } else {
                                    ty.storage_or_transient_slots(handler, contract)
                                }
                                .map(|slots| acc + slots)
                            })?;

                        // Now offset using `Add`
                        asm.push(Stack::Push(key_offset as i64).into());
                        asm.push(Alu::Add.into());
                        Ok(location)
                    }
                    Location::Value => {
                        unimplemented!("we'll handle this eventually as a fallback option")
                    }
                }
            }
            Expr::Index { expr, index, .. } => {
                let location = self.compile_expr_pointer(handler, asm, expr, contract, pred)?;
                match location {
                    Location::State(_) | Location::Transient { .. } | Location::DecisionVar(_) => {
                        // Offset calculation is pretty much the same for all types of data

                        // Grab the element ty of the array
                        let Type::Array { ty, .. } = expr.get_ty(contract) else {
                            return Err(handler.emit_err(Error::Compile {
                                error: CompileError::Internal {
                                    msg: "type must exist and be an array type",
                                    span: empty_span(),
                                },
                            }));
                        };

                        // Compile the index
                        self.compile_expr(handler, asm, index, contract, pred)?;

                        // Multiply the index by the number of storage slots for `ty` to get the
                        // offset, then add the result to the base key
                        asm.push(
                            // Decision vars are flattened in a given slots, so we look at the
                            // raw size in words. For transient and storage variables, we look
                            // at the "storage size" which may yield different results (e.g. a
                            // `b256` is 4 words but its storage size is 1
                            Stack::Push(if let Location::DecisionVar(_) = location {
                                ty.size(handler, contract)?
                            } else {
                                ty.storage_or_transient_slots(handler, contract)?
                            } as i64)
                            .into(),
                        );
                        asm.push(Alu::Mul.into());
                        asm.push(Alu::Add.into());

                        Ok(location)
                    }
                    Location::Value => {
                        unimplemented!("we'll handle this eventually as a fallback option")
                    }
                }
            }
        }
    }

    /// Generates assembly for producing a storage key  where `expr` is stored.
    ///
    /// Returns a `StorageKey`. The `StorageKey` contains two values:
    /// - The length of the storage key.
    /// - Whether the key is internal or external. External keys should be accessed using
    ///   `KeyRangeExtern`.
    fn compile_state_key(
        &self,
        handler: &Handler,
        s_asm: &mut Vec<StateRead>,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<StorageKey, ErrorEmitted> {
        match &expr.get(contract) {
            Expr::IntrinsicCall { name, args, .. } => {
                if name.name == "__storage_get" {
                    // Expecting a single argument that is an array of integers representing a key
                    assert_eq!(args.len(), 1);
                    let Ok(key_len) = args[0].get_ty(contract).size(handler, contract) else {
                        return Err(handler.emit_err(Error::Compile {
                            error: CompileError::Internal {
                                msg: "unable to get key size",
                                span: empty_span(),
                            },
                        }));
                    };

                    let mut asm = Vec::new();
                    self.compile_expr(handler, &mut asm, &args[0], contract, pred)?;
                    s_asm.extend(asm.iter().map(|op| StateRead::Constraint(*op)));
                    Ok(StorageKey {
                        len: key_len,
                        is_extern: false,
                    })
                } else if name.name == "__storage_get_extern" {
                    // Expecting two arguments:
                    // 1. An address that is a `b256`
                    // 2. A key: an array of integers
                    assert_eq!(args.len(), 2);
                    let Ok(key_len) = args[1].get_ty(contract).size(handler, contract) else {
                        return Err(handler.emit_err(Error::Compile {
                            error: CompileError::Internal {
                                msg: "unable to get key size",
                                span: empty_span(),
                            },
                        }));
                    };

                    // First, get the contract address and the storage key
                    let mut asm = Vec::new();
                    self.compile_expr(handler, &mut asm, &args[0], contract, pred)?;
                    self.compile_expr(handler, &mut asm, &args[1], contract, pred)?;
                    s_asm.extend(asm.iter().map(|op| StateRead::Constraint(*op)));
                    Ok(StorageKey {
                        len: key_len,
                        is_extern: true,
                    })
                } else {
                    unimplemented!("Other calls are currently not supported")
                }
            }
            _ => unreachable!("there really shouldn't be anything else at this stage"),
        }
    }

    /// Generates assembly for an `ExprKey`. Returns the number of opcodes used to express `expr`
    fn compile_expr(
        &self,
        handler: &Handler,
        asm: &mut Vec<Constraint>,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<usize, ErrorEmitted> {
        let old_asm_len = asm.len();
        let expr_ty = expr.get_ty(contract);
        match self.compile_expr_pointer(handler, asm, expr, contract, pred)? {
            Location::Value => {
                self.compile_value_expr(handler, asm, expr, contract, pred)?;
            }
            Location::DecisionVar(len) => {
                if len == 1 {
                    // If the decision variable itself is a single word, just `DecisionVar`
                    // directly. Otherwise, we may need `DecisionVarRange` (e.g. tuple access,
                    // etc.)
                    asm.push(Access::DecisionVar.into());
                } else {
                    asm.push(Stack::Push(expr_ty.size(handler, contract)? as i64).into()); // len
                    asm.push(Access::DecisionVarRange.into());
                }
            }
            Location::Transient { key_len, pathway } => {
                // We may need several `Transient` opcodes if the expression we're trying to read
                // is stored in multiple slots (e.g. a tuple or an array).
                for i in 0..expr_ty.storage_or_transient_slots(handler, contract)? {
                    if i > 0 {
                        // Re-compute the key and then offset with `+ i`. We do this manually here
                        // because we don't have a `TransientRange` op that is similar to
                        // `KeyRange`
                        self.compile_expr_pointer(handler, asm, expr, contract, pred)?;
                        asm.push(Stack::Push(i as i64).into());
                        asm.push(Alu::Add.into());
                    }

                    // Now just compile the key length and the pathway expression and follow with
                    // the `Transient` opcode
                    self.compile_expr(handler, asm, &key_len, contract, pred)?;
                    self.compile_expr(handler, asm, &pathway, contract, pred)?;
                    asm.push(Constraint::Access(Access::Transient));
                }
            }
            Location::State(next_state) => {
                let slots = expr_ty.storage_or_transient_slots(handler, contract)?;
                if slots == 1 {
                    asm.push(Stack::Push(next_state as i64).into());
                    asm.push(Access::State.into());
                } else {
                    asm.push(Stack::Push(slots as i64).into());
                    asm.push(Stack::Push(next_state as i64).into());
                    asm.push(Access::StateRange.into());
                }
            }
        }
        Ok(asm.len() - old_asm_len)
    }

    /// Generates assembly for an `ExprKey` that is a `Location::Value`. Returns the number of
    /// opcodes used to express `expr`
    fn compile_value_expr(
        &self,
        handler: &Handler,
        asm: &mut Vec<Constraint>,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<usize, ErrorEmitted> {
        fn compile_immediate(asm: &mut Vec<Constraint>, imm: &Immediate) {
            match imm {
                Immediate::Int(val) => asm.push(Stack::Push(*val).into()),

                Immediate::Bool(val) => asm.push(Stack::Push(*val as i64).into()),

                Immediate::B256(val) => {
                    asm.push(Stack::Push(val[0] as i64).into());
                    asm.push(Stack::Push(val[1] as i64).into());
                    asm.push(Stack::Push(val[2] as i64).into());
                    asm.push(Stack::Push(val[3] as i64).into());
                }

                Immediate::Array(elements) => {
                    for element in elements {
                        compile_immediate(asm, element);
                    }
                }

                Immediate::Tuple(fields) => {
                    for (_, field) in fields {
                        compile_immediate(asm, field);
                    }
                }

                Immediate::Error | Immediate::Nil | Immediate::Real(_) | Immediate::String(_) => {
                    unimplemented!("other literal types are not yet supported")
                }
            }
        }

        let old_asm_len = asm.len();

        match &expr.get(contract) {
            Expr::Immediate { value, .. } => compile_immediate(asm, value),
            Expr::Array { elements, .. } => {
                for element in elements {
                    self.compile_expr(handler, asm, element, contract, pred)?;
                }
            }
            Expr::Tuple { fields, .. } => {
                for (_, field) in fields {
                    self.compile_expr(handler, asm, field, contract, pred)?;
                }
            }
            Expr::BinaryOp { op, lhs, rhs, .. } => {
                let lhs_len = self.compile_expr(handler, asm, lhs, contract, pred)?;
                let rhs_len = self.compile_expr(handler, asm, rhs, contract, pred)?;
                match op {
                    BinaryOp::Add => asm.push(Alu::Add.into()),
                    BinaryOp::Sub => asm.push(Alu::Sub.into()),
                    BinaryOp::Mul => asm.push(Alu::Mul.into()),
                    BinaryOp::Div => asm.push(Alu::Div.into()),
                    BinaryOp::Mod => asm.push(Alu::Mod.into()),
                    BinaryOp::Equal => {
                        let type_size = lhs.get_ty(contract).size(handler, contract)?;
                        if type_size == 1 {
                            asm.push(Pred::Eq.into());
                        } else {
                            asm.push(Stack::Push(type_size as i64).into());
                            asm.push(Pred::EqRange.into());
                        }
                    }
                    BinaryOp::NotEqual => {
                        asm.push(Pred::Eq.into());
                        asm.push(Pred::Not.into());
                    }
                    BinaryOp::LessThanOrEqual => asm.push(Pred::Lte.into()),
                    BinaryOp::LessThan => asm.push(Pred::Lt.into()),
                    BinaryOp::GreaterThanOrEqual => {
                        asm.push(Pred::Gte.into());
                    }
                    BinaryOp::GreaterThan => asm.push(Pred::Gt.into()),
                    BinaryOp::LogicalAnd => {
                        // Short-circuit AND. Using `JumpForwardIf`, converts `x && y` to:
                        // if !x { false } else { y }

                        // Location right before the `lhs` opcodes
                        let lhs_position = asm.len() - rhs_len - lhs_len;

                        // Location right before the `rhs` opcodes
                        let rhs_position = asm.len() - rhs_len;

                        // Push `false` before `lhs` opcodes. This is the result of the `AND`
                        // operation if `lhs` is false.
                        asm.insert(lhs_position, Stack::Push(0).into());

                        // Then push the number of instructions to skip over if the `lhs` is true.
                        // That's `rhs_len + 2` because we're going to add to add `Pop` later and
                        // we want to skip over that AND all the `rhs` opcodes
                        asm.insert(lhs_position + 1, Stack::Push(rhs_len as i64 + 2).into());

                        // Now, invert `lhs` to get the jump condition which is `!lhs`
                        asm.insert(rhs_position + 2, Pred::Not.into());

                        // Then, add the `JumpForwardIf` instruction after the `rhs` opcodes and
                        // the two newly added opcodes. The `lhs` is the condition.
                        asm.insert(
                            rhs_position + 3,
                            Constraint::TotalControlFlow(TotalControlFlow::JumpForwardIf),
                        );

                        // Finally, insert a ` Pop`. The point here is that if the jump condition
                        // (i.e. `!lhs`) is false, then we want to remove the `true` we push on the
                        // stack above.
                        asm.insert(rhs_position + 4, Stack::Pop.into());
                    }
                    BinaryOp::LogicalOr => {
                        // Short-circuit OR. Using `JumpForwardIf`, converts `x || y` to:
                        // if x { true } else { y }

                        // Location right before the `lhs` opcodes
                        let lhs_position = asm.len() - rhs_len - lhs_len;

                        // Location right before the `rhs` opcodes
                        let rhs_position = asm.len() - rhs_len;

                        // Push `true` before `lhs` opcodes. This is the result of the `OR`
                        // operation if `lhs` is true.
                        asm.insert(lhs_position, Stack::Push(1).into());

                        // Then push the number of instructions to skip over if the `lhs` is true.
                        // That's `rhs_len + 2` because we're going to add to add `Pop` later and
                        // we want to skip over that AND all the `rhs` opcodes
                        asm.insert(lhs_position + 1, Stack::Push(rhs_len as i64 + 2).into());

                        // Now add the `JumpForwardIf` instruction after the `rhs` opcodes and the
                        // two newly added opcodes. The `lhs` is the condition.
                        asm.insert(
                            rhs_position + 2,
                            Constraint::TotalControlFlow(TotalControlFlow::JumpForwardIf),
                        );

                        // Then, insert a ` Pop`. The point here is that if the jump condition
                        // (i.e. `lhs`) is false, then we want to remove the `true` we push on the
                        // stack above.
                        asm.insert(rhs_position + 3, Stack::Pop.into());
                    }
                }
            }
            Expr::UnaryOp { op, expr, .. } => {
                match op {
                    UnaryOp::Not => {
                        self.compile_expr(handler, asm, expr, contract, pred)?;
                        asm.push(Pred::Not.into());
                    }
                    UnaryOp::NextState => {
                        return Err(handler.emit_err(Error::Compile {
                            error: CompileError::Internal {
                                msg: "unexpected next state expression",
                                span: empty_span(),
                            },
                        }));
                    }
                    UnaryOp::Neg => {
                        // Push `0` (i.e. `lhs`) before the `expr` (i.e. `rhs`) opcodes. Then
                        // subtract `lhs` - `rhs` to negate the value.
                        asm.push(Constraint::Stack(Stack::Push(0)));
                        self.compile_expr(handler, asm, expr, contract, pred)?;
                        asm.push(Alu::Sub.into())
                    }
                    UnaryOp::Error => unreachable!("unexpected Unary::Error"),
                }
            }
            Expr::StorageAccess { .. } | Expr::ExternalStorageAccess { .. } => {
                return Err(handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "unexpected storage access",
                        span: empty_span(),
                    },
                }));
            }
            Expr::IntrinsicCall {
                name, args, span, ..
            } => match &name.name[..] {
                "__mut_keys" => {
                    assert!(args.is_empty());
                    asm.push(Constraint::Access(Access::MutKeys));
                }

                "__this_address" => {
                    assert!(args.is_empty());
                    asm.push(Constraint::Access(Access::ThisAddress));
                }

                "__this_contract_address" => {
                    assert!(args.is_empty());
                    asm.push(Constraint::Access(Access::ThisContractAddress));
                }

                "__sibling_predicate_address" => {
                    assert_eq!(args.len(), 1);
                    assert!(args[0].get_ty(contract).is_string());
                    if let Some(Expr::Immediate {
                        value: Immediate::String(s),
                        ..
                    }) = args[0].try_get(contract)
                    {
                        // Push the sibling predicate address on the stack, one word at a time.
                        let predicate_address = self
                            .predicate_addresses
                            .get(s)
                            .expect("predicate address should exist!");

                        for word in essential_types::convert::word_4_from_u8_32(predicate_address.0)
                        {
                            asm.push(Constraint::Stack(Stack::Push(word)));
                        }
                    }
                }

                "__this_pathway" => {
                    assert!(args.is_empty());
                    asm.push(Constraint::Access(Access::ThisPathway));
                }
                "__predicate_at" => {
                    // Expecting a single argument of type int
                    assert_eq!(args.len(), 1);
                    assert!(args[0].get_ty(contract).is_int());

                    self.compile_expr(handler, asm, &args[0], contract, pred)?;
                    asm.push(Constraint::Access(Access::PredicateAt));
                }

                // Crypto ops
                "__sha256" => {
                    assert_eq!(args.len(), 1);

                    let data = args[0];
                    let data_type = &data.get_ty(contract);

                    // Compile the data argument, insert its length, and then insert the `Sha256`
                    // opcode
                    self.compile_expr(handler, asm, &data, contract, pred)?;
                    asm.push(Constraint::Stack(Stack::Push(
                        data_type.size(handler, contract)? as i64,
                    )));
                    asm.push(Constraint::Crypto(Crypto::Sha256));
                }

                "__verify_ed25519" => {
                    assert_eq!(args.len(), 3);

                    let data = args[0];
                    let signature = args[1];
                    let public_key = args[2];

                    let data_type = &data.get_ty(contract);
                    let signature_type = &signature.get_ty(contract);
                    let public_key_type = &public_key.get_ty(contract);

                    // Check argument types:
                    // - `data_type` can be anything, so nothing to check
                    // - `signature_type` must be a `{ b256, b256 }`
                    // - `public_key_type` must be a `b256`
                    let fields = signature_type
                        .get_tuple_fields()
                        .expect("expecting a tuple here");
                    assert!(fields.len() == 2 && fields[0].1.is_b256() && fields[1].1.is_b256());
                    assert!(public_key_type.is_b256());

                    // Compile all arguments separately and then insert the `VerifyEd25519` opcode
                    self.compile_expr(handler, asm, &data, contract, pred)?;
                    asm.push(Constraint::Stack(Stack::Push(
                        data_type.size(handler, contract)? as i64,
                    )));
                    self.compile_expr(handler, asm, &signature, contract, pred)?;
                    self.compile_expr(handler, asm, &public_key, contract, pred)?;
                    asm.push(Constraint::Crypto(Crypto::VerifyEd25519));
                }

                "__recover_secp256k1" => {
                    assert_eq!(args.len(), 2);

                    let data_hash = args[0];
                    let signature = args[1];

                    let data_hash_type = &data_hash.get_ty(contract);
                    let signature_type = &signature.get_ty(contract);

                    // Check argument types:
                    // - `data_hash_type` must be a `b256`
                    // - `signature_type` must be a `{ b256, b256, int }`
                    assert!(data_hash_type.is_b256());
                    let fields = signature_type
                        .get_tuple_fields()
                        .expect("expecting a tuple here");
                    assert!(
                        fields.len() == 3
                            && fields[0].1.is_b256()
                            && fields[1].1.is_b256()
                            && fields[2].1.is_int()
                    );

                    // Compile all arguments separately and then insert the `VerifyEd25519` opcode
                    self.compile_expr(handler, asm, &data_hash, contract, pred)?;
                    self.compile_expr(handler, asm, &signature, contract, pred)?;
                    asm.push(Constraint::Crypto(Crypto::RecoverSecp256k1));
                }

                "__state_len" => {
                    assert_eq!(args.len(), 1);

                    // Check argument:
                    // - `state_var` must be a path to a state var or a "next state" expression
                    assert!(match args[0].try_get(contract) {
                        Some(Expr::Path(name, _)) =>
                            pred.states().any(|(_, state)| state.name == *name),
                        Some(Expr::UnaryOp {
                            op: UnaryOp::NextState,
                            ..
                        }) => true,
                        _ => false,
                    });

                    self.compile_expr(handler, asm, &args[0], contract, pred)?;

                    // After compiling a path to a state var or a "next state" expression, we
                    // expect that the last opcode is a `State` or a `StateRange`. Pop that and
                    // replace it with `StateLen` or `StateLenRange` since we're after the state
                    // length here and not the actual state.
                    if let Some(Constraint::Access(Access::State)) = asm.last() {
                        asm.pop();
                        asm.push(Constraint::Access(Access::StateLen));
                    } else if let Some(Constraint::Access(Access::StateRange)) = asm.last() {
                        asm.pop();
                        asm.push(Constraint::Access(Access::StateLenRange));
                        // Now, add all the resulting state length. We should get back as many as
                        // we have slots for `args[0]`.
                        let slots = args[0]
                            .get_ty(contract)
                            .storage_or_transient_slots(handler, contract)?;
                        (0..slots - 1).for_each(|_| asm.push(Alu::Add.into()));
                    }
                }

                "__eq_set" => {
                    assert_eq!(args.len(), 2);
                    self.compile_expr(handler, asm, &args[0], contract, pred)?;
                    self.compile_expr(handler, asm, &args[1], contract, pred)?;
                    asm.push(Constraint::Pred(Pred::EqSet));
                }

                _ => {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "Unexpected intrinsic name",
                            span: span.clone(),
                        },
                    }));
                }
            },
            Expr::Select {
                condition,
                then_expr,
                else_expr,
                ..
            } => {
                if then_expr.can_panic(contract, pred) || else_expr.can_panic(contract, pred) {
                    // We need to short circuit these with control flow to avoid potential panics.
                    // The 'else' is put before the 'then' since it's easier to jump-if-true.
                    //
                    // This jump to 'then' will get updated with the proper distance below.
                    let to_then_jump_idx = asm.len();
                    asm.push(Constraint::Stack(Stack::Push(-1)));
                    self.compile_expr(handler, asm, condition, contract, pred)?;
                    asm.push(Constraint::TotalControlFlow(
                        TotalControlFlow::JumpForwardIf,
                    ));

                    // Compile the 'else' selection, update the prior jump.  We need to jump over
                    // the size of 'else` plus 3 instructions it uses to jump the 'then'.
                    let else_size = self.compile_expr(handler, asm, else_expr, contract, pred)?;
                    asm[to_then_jump_idx] = Constraint::Stack(Stack::Push(else_size as i64 + 4));

                    // This (unconditional) jump over 'then' will also get updated.
                    let to_end_jump_idx = asm.len();
                    asm.push(Constraint::Stack(Stack::Push(-1)));
                    asm.push(Constraint::Stack(Stack::Push(1)));
                    asm.push(Constraint::TotalControlFlow(
                        TotalControlFlow::JumpForwardIf,
                    ));

                    // Compile the 'then' selection, update the prior jump.
                    let then_size = self.compile_expr(handler, asm, then_expr, contract, pred)?;
                    asm[to_end_jump_idx] = Constraint::Stack(Stack::Push(then_size as i64 + 1));
                } else {
                    // Alternatively, evaluate both options and use ASM `select` to choose one.
                    let type_size = then_expr.get_ty(contract).size(handler, contract)?;
                    self.compile_expr(handler, asm, else_expr, contract, pred)?;
                    self.compile_expr(handler, asm, then_expr, contract, pred)?;
                    if type_size == 1 {
                        self.compile_expr(handler, asm, condition, contract, pred)?;
                        asm.push(Constraint::Stack(Stack::Select));
                    } else {
                        asm.push(Constraint::Stack(Stack::Push(type_size as i64)));
                        self.compile_expr(handler, asm, condition, contract, pred)?;
                        asm.push(Constraint::Stack(Stack::SelectRange));
                    }
                }
            }
            Expr::Path(..)
            | Expr::Error(_)
            | Expr::MacroCall { .. }
            | Expr::Index { .. }
            | Expr::TupleFieldAccess { .. }
            | Expr::Cast { .. }
            | Expr::In { .. }
            | Expr::Range { .. }
            | Expr::Generator { .. } => {
                return Err(handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "Unexpected expression during assembly generation",
                        span: empty_span(),
                    },
                }));
            }
        }
        Ok(asm.len() - old_asm_len)
    }

    /// Compile a path expression. Assumes that each path expressions corresponds to a decision
    /// variable or a state variable.
    fn compile_path(
        handler: &Handler,
        asm: &mut Vec<Constraint>,
        path: &String,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        // Handle (private) decision vars first
        if let Some((var_index, (var_key, _))) = pred
            .vars()
            .filter(|(_, var)| !var.is_pub)
            .enumerate()
            .find(|(_, (_, var))| &var.name == path)
        {
            let var_ty_size = var_key.get_ty(pred).size(handler, contract)?;
            asm.push(Stack::Push(var_index as i64).into()); // slot
            if var_key.get_ty(pred).size(handler, contract)? > 1 {
                asm.push(Stack::Push(0).into()); // placeholder for index computation
            }
            Ok(Location::DecisionVar(var_ty_size))
        }
        // Now handle state vars
        else if let Some(state_index) = pred.states().position(|(_, state)| &state.name == path) {
            asm.push(
                Stack::Push(
                    pred.states()
                        .take(state_index)
                        .try_fold(0, |acc, (state_key, _)| {
                            state_key
                                .get_ty(pred)
                                .storage_or_transient_slots(handler, contract)
                                .map(|slots| acc + slots)
                        })? as i64,
                )
                .into(),
            );

            Ok(Location::State(false))
        }
        // Must not have anything else at this point. All other path expressions should have been
        // lowered to something else by now
        else {
            return Err(handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "unexpected path expression",
                    span: empty_span(),
                },
            }));
        }
    }

    /// Generates assembly for a given constraint
    fn compile_constraint(
        &mut self,
        handler: &Handler,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<(), ErrorEmitted> {
        let mut asm = Vec::new();
        self.compile_expr(handler, &mut asm, expr, contract, pred)?;
        self.c_asm.push(asm);
        Ok(())
    }

    /// Generates assembly for a given state read
    fn compile_state(
        &mut self,
        handler: &Handler,
        state: &StateVar,
        slot_idx: &mut u32,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<(), ErrorEmitted> {
        let mut s_asm: Vec<StateRead> = Vec::new();

        // First, get the storage key
        let storage_key =
            self.compile_state_key(handler, &mut s_asm, &state.expr, contract, pred)?;

        let storage_or_transient_slots = state
            .expr
            .get_ty(contract)
            .storage_or_transient_slots(handler, contract)?;
        s_asm.extend([
            Stack::Push(storage_or_transient_slots as i64).into(),
            StateSlots::AllocSlots.into(),
            Stack::Push(storage_key.len as i64).into(), // key_len
            Stack::Push(storage_or_transient_slots as i64).into(), // num_keys_to_read
            Stack::Push(0).into(),                      // slot_index
            if storage_key.is_extern {
                StateRead::KeyRangeExtern
            } else {
                StateRead::KeyRange
            },
            TotalControlFlow::Halt.into(),
        ]);
        self.s_asm.push(s_asm);

        *slot_idx += storage_or_transient_slots as u32;
        Ok(())
    }
}

/// Converts a `crate::Predicate` into a `CompiledPredicate` which
/// includes generating assembly for the constraints and for state reads.
pub fn predicate_to_asm(
    handler: &Handler,
    contract: &Contract,
    predicate_addresses: &BTreeMap<String, ContentAddress>,
    pred: &Predicate,
) -> Result<CompiledPredicate, ErrorEmitted> {
    let mut builder = AsmBuilder {
        s_asm: Vec::new(),
        c_asm: Vec::new(),
        predicate_addresses,
    };

    let mut slot_idx = 0;
    for (_, state) in pred.states() {
        let _ = builder.compile_state(handler, state, &mut slot_idx, contract, pred);
    }

    for ConstraintDecl {
        expr: constraint, ..
    } in &pred.constraints
    {
        let _ = builder.compile_constraint(handler, constraint, contract, pred);
    }

    if handler.has_errors() {
        return Err(handler.cancel());
    }

    Ok(CompiledPredicate {
        state_read: builder
            .s_asm
            .iter()
            .map(|s_asm| state_asm::to_bytes(s_asm.iter().copied()).collect())
            .collect(),
        constraints: builder
            .c_asm
            .iter()
            .map(|c_asm| constraint_asm::to_bytes(c_asm.iter().copied()).collect())
            .collect(),
        directive: Directive::Satisfy,
    })
}
