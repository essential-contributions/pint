use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{BinaryOp, Expr, Immediate, TupleAccess, UnaryOp},
    intermediate::{
        ConstraintDecl, ExprKey, IntentInstance, IntermediateIntent, Program, ProgramKind,
        State as StateVar,
    },
    span::empty_span,
    types::Type,
};
use essential_types::intent::{Directive, Intent};
use state_asm::{
    Access, Alu, Constraint, ControlFlow, Crypto, Op as StateRead, Pred, Stack, StateSlots,
    TotalControlFlow,
};

mod display;
#[cfg(test)]
mod tests;

#[derive(Debug, Default, Clone)]
pub struct Intents {
    pub kind: ProgramKind,
    pub names: Vec<String>,
    pub intents: Vec<Intent>,
}

impl Intents {
    pub const ROOT_INTENT_NAME: &'static str = "";

    /// The root intent is the one named `Intents::ROOT_INTENT_NAME`
    pub fn root_intent(&self) -> &Intent {
        &self.intents[self
            .names
            .iter()
            .position(|name| name == Self::ROOT_INTENT_NAME)
            .unwrap()]
    }
}

/// Convert a `Program` into `Intents`
pub fn program_to_intents(handler: &Handler, program: &Program) -> Result<Intents, ErrorEmitted> {
    let mut names = Vec::new();
    let mut intents = Vec::new();
    match program.kind {
        ProgramKind::Stateless => {
            let (name, ii) = program.iis.iter().next().unwrap();
            if let Ok(intent) = handler.scope(|handler| intent_to_asm(handler, ii)) {
                names.push(name.to_string());
                intents.push(intent);
            }
        }
        ProgramKind::Stateful => {
            for (name, ii) in program.iis.iter() {
                if name != Program::ROOT_II_NAME {
                    if let Ok(intent) = handler.scope(|handler| intent_to_asm(handler, ii)) {
                        names.push(name.to_string());
                        intents.push(intent);
                    }
                }
            }
        }
    }

    if handler.has_errors() {
        return Err(handler.cancel());
    }

    Ok(Intents {
        kind: program.kind.clone(),
        names,
        intents,
    })
}

#[derive(Default)]
pub struct AsmBuilder {
    // Opcodes to read state.
    s_asm: Vec<Vec<StateRead>>,
    // Opcodes to specify constraints
    c_asm: Vec<Vec<Constraint>>,
}

#[derive(Debug)]
enum StorageKeyKind {
    Static(Vec<i64>),
    Dynamic(usize),
}

#[derive(Debug)]
struct StorageKey {
    kind: StorageKeyKind,
    is_extern: bool,
}

impl StorageKey {
    fn len(&self) -> usize {
        match &self.kind {
            StorageKeyKind::Static(key) => key.len(),
            StorageKeyKind::Dynamic(len) => *len,
        }
    }
}

impl AsmBuilder {
    /// Generates assembly for producing a storage key  where `expr` is stored.
    /// Returns a `StorageKey`. The `StorageKey` contains two values:
    /// - The kind of the storage key: static where the keys are known at compile-time or dynamic.
    /// - Whether the key is internal or external. External keys should be accessed using
    /// `StateReadKeyRangeExtern`.
    fn compile_state_key(
        handler: &Handler,
        s_asm: &mut Vec<StateRead>,
        expr: &ExprKey,
        intent: &IntermediateIntent,
    ) -> Result<StorageKey, ErrorEmitted> {
        let expr_ty = expr.get_ty(intent);
        match &expr.get(intent) {
            Expr::IntrinsicCall { name, args, .. } => {
                if name.name.ends_with("__storage_get") {
                    // Expecting a single argument that is an array of integers representing a key
                    assert_eq!(args.len(), 1);
                    let Some(key_size) = args[0].get_ty(intent).get_array_size() else {
                        return Err(handler.emit_err(Error::Compile {
                            error: CompileError::Internal {
                                msg: "unable to get key size",
                                span: empty_span(),
                            },
                        }));
                    };

                    let mut asm = Vec::new();
                    Self::compile_expr(handler, &mut asm, &args[0], intent)?;
                    s_asm.extend(asm.iter().map(|op| StateRead::Constraint(*op)));
                    Ok(StorageKey {
                        kind: StorageKeyKind::Dynamic(key_size as usize),
                        is_extern: false,
                    })
                } else if name.name.ends_with("__storage_get_extern") {
                    // Expecting two arguments:
                    // 1. An address that is a `b256`
                    // 2. A key: an array of integers
                    assert_eq!(args.len(), 2);
                    let Some(key_size) = args[1].get_ty(intent).get_array_size() else {
                        return Err(handler.emit_err(Error::Compile {
                            error: CompileError::Internal {
                                msg: "unable to get key size",
                                span: empty_span(),
                            },
                        }));
                    };

                    // First, get the set-of-intents address and the storage key
                    let mut asm = Vec::new();
                    Self::compile_expr(handler, &mut asm, &args[0], intent)?;
                    Self::compile_expr(handler, &mut asm, &args[1], intent)?;
                    s_asm.extend(asm.iter().map(|op| StateRead::Constraint(*op)));
                    Ok(StorageKey {
                        kind: StorageKeyKind::Dynamic(key_size as usize),
                        is_extern: true,
                    })
                } else {
                    unimplemented!("Other calls are currently not supported")
                }
            }
            Expr::StorageAccess(name, _) => {
                let storage = &intent
                    .storage
                    .as_ref()
                    .expect("a storage block must have been declared")
                    .0;

                // Get the index of the storage variable in the storage block declaration
                let storage_index = storage
                    .iter()
                    .position(|var| var.name == *name)
                    .expect("storage access should have been checked before");

                // This is the key. It's either the `storage_index` if the storage type primitive
                // or a map, or it's `[storage_index, 0]`. The `0` here is a placeholder for
                // offsets.
                let storage_var = &storage[storage_index];
                let key = if storage_var.ty.is_any_primitive() || storage_var.ty.is_map() {
                    s_asm.push(Stack::Push(storage_index as i64).into());
                    vec![storage_index as i64]
                } else {
                    s_asm.push(Stack::Push(storage_index as i64).into());
                    s_asm.push(Stack::Push(0).into());
                    vec![storage_index as i64, 0]
                };

                Ok(StorageKey {
                    kind: StorageKeyKind::Static(key),
                    is_extern: false,
                })
            }
            Expr::ExternalStorageAccess {
                interface_instance,
                name,
                ..
            } => {
                // Get the `interface_instance` declaration that the storage access refers to
                let interface_instance = &intent
                    .interface_instances
                    .iter()
                    .find(|e| e.name.to_string() == *interface_instance)
                    .expect("missing interface instance");

                // Compile the interface instance address
                let mut asm = Vec::new();
                Self::compile_expr(handler, &mut asm, &interface_instance.address, intent)?;
                s_asm.extend(asm.iter().map(|op| StateRead::Constraint(*op)));

                // Get the `interface` declaration that the storage access refers to
                let interface = &intent
                    .interfaces
                    .iter()
                    .find(|e| e.name.to_string() == *interface_instance.interface)
                    .expect("missing interface");

                // Get the index of the storage variable in the storage block declaration
                let storage = &interface
                    .storage
                    .as_ref()
                    .expect("a storage block must have been declared")
                    .0;

                let storage_index = storage
                    .iter()
                    .position(|var| var.name == *name)
                    .expect("storage access should have been checked before");

                // This is the key. It's either the `storage_index` if the storage type primitive
                // or a map, or it's `[storage_index, 0]`. The `0` here is a placeholder for
                // offsets.
                let storage_var = &storage[storage_index];
                let key = if storage_var.ty.is_any_primitive() || storage_var.ty.is_map() {
                    s_asm.push(Stack::Push(storage_index as i64).into());
                    vec![storage_index as i64]
                } else {
                    s_asm.push(Stack::Push(storage_index as i64).into());
                    s_asm.push(Stack::Push(0).into());
                    vec![storage_index as i64, 0]
                };

                // This is external!
                Ok(StorageKey {
                    kind: StorageKeyKind::Static(key),
                    is_extern: true,
                })
            }
            Expr::Index { expr, index, .. } => {
                // Compile the key corresponding to `expr`
                let storage_key = Self::compile_state_key(handler, s_asm, expr, intent)?;

                // Compile the index
                let mut asm = vec![];
                Self::compile_expr(handler, &mut asm, index, intent)?;
                s_asm.extend(asm.iter().copied().map(StateRead::from));
                let mut key_length = storage_key.len() + index.get_ty(intent).size();
                if !(expr_ty.is_any_primitive() || expr_ty.is_map()) {
                    s_asm.push(StateRead::from(Stack::Push(0)));
                    key_length += 1;
                }
                Ok(StorageKey {
                    kind: StorageKeyKind::Dynamic(key_length),
                    is_extern: storage_key.is_extern,
                })
            }
            Expr::TupleFieldAccess { tuple, field, .. } => {
                // Compile the key corresponding to `tuple`
                let StorageKey { kind, is_extern } =
                    Self::compile_state_key(handler, s_asm, tuple, intent)?;

                // Grab the fields of the tuple
                let Type::Tuple { ref fields, .. } = tuple.get_ty(intent) else {
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
                let key_offset: usize = fields
                    .iter()
                    .take(field_idx)
                    .map(|(_, ty)| ty.storage_slots())
                    .sum();

                // Increment the last word on the satck by `key_offset`. This works fine for the
                // static case because all static keys start at zero (at least for now). For
                // dynamic keys, this is not accurate due to a potential overflow. I'm going to
                // keep this for now though so that we can keep things going, but we need a proper
                // solution (`Add4` opcode, or storage reads with offset, or even a whole new
                // storage design that uses b-trees.)
                Ok(StorageKey {
                    kind: match kind {
                        StorageKeyKind::Dynamic(size) => {
                            // Increment the last word on the stack by `key_offset`
                            s_asm.push(Stack::Push(key_offset as i64).into());
                            s_asm.push(Alu::Add.into());
                            StorageKeyKind::Dynamic(size)
                        }
                        StorageKeyKind::Static(mut key) => {
                            // Remove the last word on the stack and increment it by `key_offset`.
                            s_asm.push(Stack::Pop.into());
                            let len = key.len();
                            key[len - 1] += key_offset as i64;
                            s_asm.push(Stack::Push(key[len - 1]).into());
                            StorageKeyKind::Static(key)
                        }
                    },
                    is_extern,
                })
            }
            _ => unreachable!("there really shouldn't be anything else at this stage"),
        }
    }

    /// Generates assembly for an `ExprKey`. Returns the number of opcodes used to express `expr`
    fn compile_expr(
        handler: &Handler,
        asm: &mut Vec<Constraint>,
        expr: &ExprKey,
        intent: &IntermediateIntent,
    ) -> Result<usize, ErrorEmitted> {
        fn compile_immediate(asm: &mut Vec<Constraint>, imm: &Immediate) {
            match imm {
                Immediate::Int(val) => asm.push(Stack::Push(*val).into()),

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

                Immediate::Error
                | Immediate::Nil
                | Immediate::Real(_)
                | Immediate::Bool(_)
                | Immediate::String(_) => {
                    unimplemented!("other literal types are not yet supported")
                }
            }
        }

        let old_asm_len = asm.len();
        // Always push to the vector of ops corresponding to the last constraint, i.e. the current
        // constraint being processed.
        //
        // Assume that there exists at least a single entry in `self.c_asm`.
        match &expr.get(intent) {
            Expr::Immediate { value, .. } => compile_immediate(asm, value),
            Expr::Array { elements, .. } => {
                for element in elements {
                    Self::compile_expr(handler, asm, element, intent)?;
                }
            }
            Expr::Tuple { fields, .. } => {
                for (_, field) in fields {
                    Self::compile_expr(handler, asm, field, intent)?;
                }
            }
            Expr::BinaryOp { op, lhs, rhs, .. } => {
                let lhs_len = Self::compile_expr(handler, asm, lhs, intent)?;
                let rhs_len = Self::compile_expr(handler, asm, rhs, intent)?;
                match op {
                    BinaryOp::Add => asm.push(Alu::Add.into()),
                    BinaryOp::Sub => asm.push(Alu::Sub.into()),
                    BinaryOp::Mul => asm.push(Alu::Mul.into()),
                    BinaryOp::Div => asm.push(Alu::Div.into()),
                    BinaryOp::Mod => asm.push(Alu::Mod.into()),
                    BinaryOp::Equal => {
                        let type_size = lhs.get_ty(intent).size();
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
                        // That's `rhs_len + 2` because we're goint to add to add `Pop` later and
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
                        // That's `rhs_len + 2` because we're goint to add to add `Pop` later and
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
                Self::compile_expr(handler, asm, expr, intent)?;
                match op {
                    UnaryOp::Not => {
                        asm.push(Pred::Not.into());
                    }
                    UnaryOp::NextState => {
                        // This assumes that the next state operator is applied on a state var path
                        // directly which should currently be guaranteed by the middleend.
                        //
                        // So, we simply change the second the last instruction to `Push(1)`
                        // instead of `Push(0)`. This changes the `delta` for the state read
                        // instruction from 0 to 1. We're basically switching from reading the
                        // current state to reading the next state.
                        assert!(matches!(
                            asm.last(),
                            Some(&Constraint::Access(Access::State | Access::StateRange))
                        ));
                        let len = asm.len();
                        assert!(len >= 2);
                        assert!(matches!(
                            asm.get(asm.len() - 2),
                            Some(&Constraint::Stack(Stack::Push(0)))
                        ));
                        asm[len - 2] = Stack::Push(1).into();
                    }
                    UnaryOp::Neg => unimplemented!("Unary::Neg is not yet supported"),
                    UnaryOp::Error => unreachable!("unexpected Unary::Error"),
                }
            }
            Expr::PathByName(path, _) => {
                // Search for a decision variable or a state variable.
                Self::compile_path(handler, asm, &path.to_string(), intent)?;
            }
            Expr::PathByKey(var_key, _) => {
                // Search for a decision variable or a state variable.
                Self::compile_path(handler, asm, &var_key.get(intent).name, intent)?;
            }
            Expr::StorageAccess(_, _) | Expr::ExternalStorageAccess { .. } => {
                return Err(handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "unexpected storage access",
                        span: empty_span(),
                    },
                }));
            }
            Expr::IntrinsicCall { name, args, span } => match &name.name[..] {
                // Access ops
                "__mut_keys_len" => {
                    assert!(args.is_empty());
                    asm.push(Constraint::Access(Access::MutKeysLen));
                }

                "__mut_keys_contains" => {
                    assert_eq!(args.len(), 1);

                    let mut_key = args[0];
                    let mut_key_type = &mut_key.get_ty(intent);

                    // Check that the mutable key is an array of integers
                    let el_ty = mut_key_type.get_array_el_type().unwrap();
                    assert!(el_ty.is_int());

                    // Compile the mut key argument, insert its length, and then insert the
                    // `Sha256` opcode
                    Self::compile_expr(handler, asm, &mut_key, intent)?;
                    asm.push(Constraint::Stack(Stack::Push(mut_key_type.size() as i64)));
                    asm.push(Constraint::Access(Access::MutKeysContains));
                }

                "__this_address" => {
                    assert!(args.is_empty());
                    asm.push(Constraint::Access(Access::ThisAddress));
                }

                "__this_set_address" => {
                    assert!(args.is_empty());
                    asm.push(Constraint::Access(Access::ThisSetAddress));
                }

                "__this_pathway" => {
                    assert!(args.is_empty());
                    asm.push(Constraint::Access(Access::ThisPathway));
                }

                // Crypto ops
                "__sha256" => {
                    assert_eq!(args.len(), 1);

                    let data = args[0];
                    let data_type = &data.get_ty(intent);

                    // Compile the data argument, insert its length, and then insert the `Sha256`
                    // opcode
                    Self::compile_expr(handler, asm, &data, intent)?;
                    asm.push(Constraint::Stack(Stack::Push(data_type.size() as i64)));
                    asm.push(Constraint::Crypto(Crypto::Sha256));
                }

                "__verify_ed25519" => {
                    assert_eq!(args.len(), 3);

                    let data = args[0];
                    let signature = args[1];
                    let public_key = args[2];

                    let data_type = &data.get_ty(intent);
                    let signature_type = &signature.get_ty(intent);
                    let public_key_type = &public_key.get_ty(intent);

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
                    Self::compile_expr(handler, asm, &data, intent)?;
                    asm.push(Constraint::Stack(Stack::Push(data_type.size() as i64)));
                    Self::compile_expr(handler, asm, &signature, intent)?;
                    Self::compile_expr(handler, asm, &public_key, intent)?;
                    asm.push(Constraint::Crypto(Crypto::VerifyEd25519));
                }

                "__recover_secp256k1" => {
                    assert_eq!(args.len(), 2);

                    let data_hash = args[0];
                    let signature = args[1];

                    let data_hash_type = &data_hash.get_ty(intent);
                    let signature_type = &signature.get_ty(intent);

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
                    Self::compile_expr(handler, asm, &data_hash, intent)?;
                    Self::compile_expr(handler, asm, &signature, intent)?;
                    asm.push(Constraint::Crypto(Crypto::RecoverSecp256k1));
                }

                "__state_len" => {
                    assert_eq!(args.len(), 1);

                    // Check argument:
                    // - `state_var` must be a path to a state var or a "next state" expression
                    assert!(match args[0].try_get(intent) {
                        Some(Expr::PathByName(name, _))
                            if intent.states().any(|(_, state)| state.name == *name) =>
                            true,
                        Some(Expr::PathByKey(var_key, _))
                            if intent
                                .states()
                                .any(|(_, state)| state.name == var_key.get(intent).name) =>
                            true,
                        Some(Expr::UnaryOp {
                            op: UnaryOp::NextState,
                            ..
                        }) => true,
                        _ => false,
                    });

                    Self::compile_expr(handler, asm, &args[0], intent)?;

                    // After compiling a path to a state var or a "next state" expression, we
                    // expect that the last opcode is a `State` or a `StateRange`. Pop that and
                    // replace it with `StateLen` since we're after the state length here and not
                    // the actual state.
                    assert!(matches!(
                        asm.last(),
                        Some(&Constraint::Access(Access::State | Access::StateRange))
                    ));
                    asm.pop();

                    asm.push(Constraint::Access(Access::StateLen));
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
                let type_size = then_expr.get_ty(intent).size();
                Self::compile_expr(handler, asm, else_expr, intent)?;
                Self::compile_expr(handler, asm, then_expr, intent)?;
                if type_size == 1 {
                    Self::compile_expr(handler, asm, condition, intent)?;
                    asm.push(Constraint::Stack(Stack::Select));
                } else {
                    asm.push(Constraint::Stack(Stack::Push(type_size as i64)));
                    Self::compile_expr(handler, asm, condition, intent)?;
                    asm.push(Constraint::Stack(Stack::SelectRange));
                }
            }
            Expr::Error(_)
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
        intent: &IntermediateIntent,
    ) -> Result<(), ErrorEmitted> {
        let var_index = intent
            .vars()
            .filter(|(_, var)| !var.is_pub)
            .position(|(_, var)| &var.name == path);
        let pub_var_index = intent
            .vars()
            .filter(|(_, var)| var.is_pub)
            .position(|(_, var)| &var.name == path);
        let state_and_index = intent
            .states()
            .enumerate()
            .find(|(_, state)| &state.1.name == path);

        match (var_index, pub_var_index, state_and_index) {
            (Some(var_index), None, None) => {
                let var_key = intent.vars().find(|(_, var)| &var.name == path).unwrap().0;
                let var_ty_size = var_key.get_ty(intent).size();
                asm.push(Stack::Push(var_index as i64).into()); // slot
                if var_ty_size == 1 {
                    asm.push(Access::DecisionVar.into());
                } else {
                    asm.push(Stack::Push(0).into()); // index
                    asm.push(Stack::Push(var_ty_size as i64).into()); // len
                    asm.push(Access::DecisionVarRange.into());
                }
            }
            (None, Some(pub_var_index), None) => {
                let var_key = intent.vars().find(|(_, var)| &var.name == path).unwrap().0;
                if var_key.get_ty(intent).is_any_primitive() {
                    asm.push(Stack::Push(pub_var_index as i64).into());
                    asm.push(Stack::Push(1).into()); // key length
                    asm.push(Constraint::Access(Access::ThisPathway));
                    asm.push(Constraint::Access(Access::Transient));
                } else {
                    todo!("non-primitive transient data not yet implemented")
                }
            }
            (None, None, Some((state_index, (state_key, _)))) => {
                let slot_index: usize = intent
                    .states()
                    .take(state_index)
                    .map(|(state_key, _)| state_key.get_ty(intent).storage_slots())
                    .sum();

                let slots = state_key.get_ty(intent).storage_slots();
                if slots == 1 {
                    asm.push(Stack::Push(slot_index as i64).into());
                    asm.push(Stack::Push(0).into()); // 0 means "current state"
                    asm.push(Access::State.into());
                } else {
                    asm.push(Stack::Push(slot_index as i64).into());
                    asm.push(Stack::Push(slots as i64).into()); // 0 means "current state"
                    asm.push(Stack::Push(0).into()); // 0 means "current state"
                    asm.push(Access::StateRange.into());
                }
            }
            _ => {
                // try external vars by looking through all available intent instances and their
                // corresponding interfaces
                let mut pub_var_found = false;
                for IntentInstance {
                    name,
                    interface_instance,
                    intent: intent_name,
                    ..
                } in &intent.intent_instances
                {
                    let Some(interface_instance) = intent
                        .interface_instances
                        .iter()
                        .find(|e| e.name.to_string() == *interface_instance)
                    else {
                        continue;
                    };

                    let Some(interface) = intent
                        .interfaces
                        .iter()
                        .find(|e| e.name.to_string() == *interface_instance.interface)
                    else {
                        continue;
                    };

                    let Some(intent_interface) = interface
                        .intent_interfaces
                        .iter()
                        .find(|e| e.name.to_string() == *intent_name.to_string())
                    else {
                        continue;
                    };

                    let Some(transient_index) = intent_interface
                        .vars
                        .iter()
                        .position(|var| name.to_string() + "::" + &var.name == *path)
                    else {
                        continue;
                    };

                    let Some(var) = intent_interface
                        .vars
                        .iter()
                        .find(|var| name.to_string() + "::" + &var.name == *path)
                    else {
                        continue;
                    };

                    if var.ty.is_any_primitive() {
                        asm.push(Stack::Push(transient_index as i64).into());
                        asm.push(Stack::Push(1).into()); // key length
                        Self::compile_path(
                            handler,
                            asm,
                            &("__".to_owned() + &name.to_string() + "_pathway"),
                            intent,
                        )?;
                        asm.push(Constraint::Access(Access::Transient));
                    } else {
                        unimplemented!("non-primitive transient data not yet implemented")
                    }
                    pub_var_found = true;
                    break;
                }
                if !pub_var_found {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "unable to find external pub var",
                            span: empty_span(),
                        },
                    }));
                }
            }
        }
        Ok(())
    }

    /// Generates assembly for a given constraint
    fn compile_constraint(
        &mut self,
        handler: &Handler,
        expr: &ExprKey,
        intent: &IntermediateIntent,
    ) -> Result<(), ErrorEmitted> {
        let mut asm = Vec::new();
        Self::compile_expr(handler, &mut asm, expr, intent)?;
        self.c_asm.push(asm);
        Ok(())
    }

    /// Generates assembly for a given state read
    fn compile_state(
        &mut self,
        handler: &Handler,
        state: &StateVar,
        slot_idx: &mut u32,
        intent: &IntermediateIntent,
    ) -> Result<(), ErrorEmitted> {
        let mut s_asm: Vec<StateRead> = Vec::new();

        // First, get the storage key
        let storage_key = Self::compile_state_key(handler, &mut s_asm, &state.expr, intent)?;
        let key_len = match storage_key.kind {
            StorageKeyKind::Static(key) => key.len(),
            StorageKeyKind::Dynamic(size) => size,
        };

        let storage_slots = state.expr.get_ty(intent).storage_slots();
        s_asm.extend([
            Stack::Push(storage_slots as i64).into(),
            StateSlots::AllocSlots.into(),
            Stack::Push(key_len as i64).into(),       // key_len
            Stack::Push(storage_slots as i64).into(), // num_keys_to_read
            Stack::Push(0).into(),                    // slot_index
            if storage_key.is_extern {
                StateRead::KeyRangeExtern
            } else {
                StateRead::KeyRange
            },
            StateRead::ControlFlow(ControlFlow::Halt),
        ]);
        self.s_asm.push(s_asm);

        *slot_idx += storage_slots as u32;
        Ok(())
    }
}

/// Converts a `crate::IntermediateIntent` into a `Intent` which
/// includes generating assembly for the constraints and for state reads.
pub fn intent_to_asm(
    handler: &Handler,
    final_intent: &IntermediateIntent,
) -> Result<Intent, ErrorEmitted> {
    let mut builder = AsmBuilder::default();

    let mut slot_idx = 0;
    for (_, state) in final_intent.states() {
        let _ = builder.compile_state(handler, state, &mut slot_idx, final_intent);
    }

    for ConstraintDecl {
        expr: constraint, ..
    } in &final_intent.constraints
    {
        let _ = builder.compile_constraint(handler, constraint, final_intent);
    }

    if handler.has_errors() {
        return Err(handler.cancel());
    }

    Ok(Intent {
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
