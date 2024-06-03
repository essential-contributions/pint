use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{BinaryOp, Expr, Immediate, TupleAccess, UnaryOp},
    intermediate::{
        ConstraintDecl, ExprKey, IntermediateIntent, Program, ProgramKind, State as StateVar,
    },
    span::empty_span,
    types::Type,
};
use essential_types::intent::{Directive, Intent};
use state_asm::{
    Access, Alu, Constraint, ControlFlow, Crypto, Op as StateRead, Pred, Stack, StateSlots,
};
use std::collections::HashMap;

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
    // Maps indices of `let` variables (which may be wider than a word) to a list of low level
    // word-wide decision variables
    var_to_d_vars: HashMap<usize, Vec<usize>>,
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
        &mut self,
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
                    self.compile_expr(handler, &mut asm, &args[0], intent)?;
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
                    self.compile_expr(handler, &mut asm, &args[0], intent)?;
                    self.compile_expr(handler, &mut asm, &args[1], intent)?;
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
                self.compile_expr(handler, &mut asm, &interface_instance.address, intent)?;
                s_asm.extend(asm.iter().map(|op| StateRead::Constraint(*op)));

                // Get the `interface` declaration that the storage access refers to
                let interface = &intent
                    .interfaces
                    .iter()
                    .find(|e| e.name.to_string() == *interface_instance.interface)
                    .expect("missing interface");

                // Get the index of the storage variable in the storage block declaration
                let storage_index = interface
                    .storage_vars
                    .iter()
                    .position(|var| var.name == *name)
                    .expect("storage access should have been checked before");

                // This is the key. It's either the `storage_index` if the storage type primitive
                // or a map, or it's `[storage_index, 0]`. The `0` here is a placeholder for
                // offsets.
                let storage_var = &interface.storage_vars[storage_index];
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
                let storage_key = self.compile_state_key(handler, s_asm, expr, intent)?;

                // Compile the index
                let mut asm = vec![];
                self.compile_expr(handler, &mut asm, index, intent)?;
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
                    self.compile_state_key(handler, s_asm, tuple, intent)?;

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

    /// Generates assembly for an `ExprKey`.
    fn compile_expr(
        &mut self,
        handler: &Handler,
        asm: &mut Vec<Constraint>,
        expr: &ExprKey,
        intent: &IntermediateIntent,
    ) -> Result<(), ErrorEmitted> {
        // Always push to the vector of ops corresponding to the last constraint, i.e. the current
        // constraint being processed.
        //
        // Assume that there exists at least a single entry in `self.c_asm`.
        match &expr.get(intent) {
            Expr::Immediate { value, .. } => match value {
                Immediate::Int(val) => asm.push(Stack::Push(*val).into()),
                Immediate::B256(val) => {
                    asm.push(Stack::Push(val[0] as i64).into());
                    asm.push(Stack::Push(val[1] as i64).into());
                    asm.push(Stack::Push(val[2] as i64).into());
                    asm.push(Stack::Push(val[3] as i64).into());
                }
                _ => unimplemented!("other literal types are not yet supported"),
            },
            Expr::BinaryOp { op, lhs, rhs, .. } => {
                self.compile_expr(handler, asm, lhs, intent)?;
                self.compile_expr(handler, asm, rhs, intent)?;
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
                    BinaryOp::LogicalAnd => asm.push(Pred::And.into()),
                    BinaryOp::LogicalOr => asm.push(Pred::Or.into()),
                }
            }
            Expr::UnaryOp { op, expr, .. } => {
                self.compile_expr(handler, asm, expr, intent)?;
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
                self.compile_path(asm, &path.to_string(), intent);
            }
            Expr::PathByKey(var_key, _) => {
                // Search for a decision variable or a state variable.
                self.compile_path(asm, &var_key.get(intent).name, intent);
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
                    self.compile_expr(handler, asm, &mut_key, intent)?;
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
                    self.compile_expr(handler, asm, &data, intent)?;
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
                    self.compile_expr(handler, asm, &data, intent)?;
                    asm.push(Constraint::Stack(Stack::Push(data_type.size() as i64)));
                    self.compile_expr(handler, asm, &signature, intent)?;
                    self.compile_expr(handler, asm, &public_key, intent)?;
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
                    self.compile_expr(handler, asm, &data_hash, intent)?;
                    self.compile_expr(handler, asm, &signature, intent)?;
                    asm.push(Constraint::Crypto(Crypto::RecoverSecp256k1));
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
            Expr::Tuple { fields, .. } => {
                fields
                    .iter()
                    .try_for_each(|(_, field)| self.compile_expr(handler, asm, field, intent))?;
            }
            Expr::Array { elements, .. } => {
                elements
                    .iter()
                    .try_for_each(|element| self.compile_expr(handler, asm, element, intent))?;
            }
            Expr::Select {
                condition,
                then_expr,
                else_expr,
                ..
            } => {
                let type_size = then_expr.get_ty(intent).size();
                self.compile_expr(handler, asm, else_expr, intent)?;
                self.compile_expr(handler, asm, then_expr, intent)?;
                self.compile_expr(handler, asm, condition, intent)?;
                if type_size == 1 {
                    asm.push(Constraint::Stack(Stack::Select));
                } else {
                    // `SelectRange` when it's available
                    todo!()
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
        Ok(())
    }

    /// Compile a path expression. Assumes that each path expressions corresponds to a decision
    /// variable or a state variable.
    fn compile_path(
        &mut self,
        asm: &mut Vec<Constraint>,
        path: &String,
        intent: &IntermediateIntent,
    ) {
        let var_index = intent.vars().position(|(_, var)| &var.name == path);
        let state_and_index = intent
            .states()
            .enumerate()
            .find(|(_, state)| &state.1.name == path);

        match (var_index, state_and_index) {
            (Some(var_index), None) => {
                for d_var in &self.var_to_d_vars[&var_index] {
                    asm.push(Stack::Push(*d_var as i64).into());
                    asm.push(Access::DecisionVar.into());
                }
            }
            (None, Some((state_index, (state_key, _)))) => {
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
            _ => unreachable!("guaranteed by semantic analysis"),
        }
    }

    /// Generates assembly for a given constraint
    fn compile_constraint(
        &mut self,
        handler: &Handler,
        expr: &ExprKey,
        intent: &IntermediateIntent,
    ) -> Result<(), ErrorEmitted> {
        let mut asm = Vec::new();
        self.compile_expr(handler, &mut asm, expr, intent)?;
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
        let storage_key = self.compile_state_key(handler, &mut s_asm, &state.expr, intent)?;
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

    // Low level decision variable index
    //
    // This assumes that all decision variables are either `b256` or have size 1 word, as a result
    // of flattening.
    let mut d_var = 0;
    for (idx, (var_key, _)) in final_intent.vars().enumerate() {
        if var_key.get_ty(final_intent).is_b256() {
            // `B256` variables map to 4 separate low level decision variables, 1 word wide each.
            builder
                .var_to_d_vars
                .insert(idx, vec![d_var, d_var + 1, d_var + 2, d_var + 3]);
            d_var += 4;
        } else {
            // All other primitive types (ignoring strings) are 1 word wide.
            builder.var_to_d_vars.insert(idx, vec![d_var]);
            d_var += 1;
        }
    }

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
