use super::Inference;
use super::{Contract, ExprKey, Ident, Predicate};
use crate::error::Handler;
use crate::{
    error::{CompileError, Error},
    expr::{Expr, UnaryOp},
    span::{empty_span, Span, Spanned},
    types::{PrimitiveKind, Type},
};

impl Contract {
    pub(super) fn infer_intrinsic_call_expr(
        &self,
        handler: &Handler,
        pred: Option<&Predicate>,
        name: &Ident,
        args: &[ExprKey],
        span: &Span,
    ) -> Result<Inference, Error> {
        let mut deps = Vec::new();

        args.iter()
            .filter(|arg_key| arg_key.get_ty(self).is_unknown())
            .for_each(|arg_key| deps.push(*arg_key));

        if deps.is_empty() {
            match &name.name[..] {
                // Access ops
                "__mut_keys_len" => Ok(infer_intrinsic_mut_keys_len(handler, args, span)),
                "__mut_keys_contains" => Ok(infer_intrinsic_mut_keys_contains(
                    self, handler, name, args, span,
                )),
                "__this_address" => Ok(infer_intrinsic_this_address(handler, args, span)),
                "__this_set_address" => Ok(infer_intrinsic_this_set_address(handler, args, span)),
                "__this_pathway" => Ok(infer_intrinsic_this_pathway(handler, args, span)),

                // Crypto ops
                "__sha256" => Ok(infer_intrinsic_sha256(handler, args, span)),
                "__verify_ed25519" => Ok(infer_intrinsic_verify_ed25519(
                    self, handler, name, args, span,
                )),
                "__recover_secp256k1" => Ok(infer_intrinsic_recover_secp256k1(
                    self, handler, name, args, span,
                )),

                "__state_len" => Ok(infer_intrinsic_state_len(self, handler, pred, args, span)),
                "__vec_len" => Ok(infer_intrinsic_vec_len(self, handler, pred, args, span)),

                // State reads - these will likely change in the future as they don't directly
                // match the underlying opcodes
                "__storage_get" | "__storage_get_extern" => Ok(Inference::Type(Type::Primitive {
                    kind: PrimitiveKind::Int,
                    span: span.clone(),
                })),

                _ => Err(Error::Compile {
                    error: CompileError::MissingIntrinsic {
                        name: name.name.clone(),
                        span: span.clone(),
                    },
                }),
            }
        } else {
            Ok(Inference::Dependencies(deps))
        }
    }
}

//
// Intrinsic `__mut_keys_len`
//
// - Arguments: none
//
// - Returns: `int`
//
// Description: Get the number of mutable keys being proposed for mutation.
//
fn infer_intrinsic_mut_keys_len(handler: &Handler, args: &[ExprKey], span: &Span) -> Inference {
    // This intrinsic expects no arguments
    if !args.is_empty() {
        handler.emit_err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 0,
                found: args.len(),
                span: span.clone(),
            },
        });
    }

    // This intrinsic returns a `int`
    Inference::Type(Type::Primitive {
        kind: PrimitiveKind::Int,
        span: span.clone(),
    })
}

//
// Intrinsic `__mut_keys_contains`
//
// - Arguments:
//     * Key: `int[n]` for some `n`.
//
// - Returns: `bool`
//
// Description: Check if the mutable keys being proposed contain the given key
//
fn infer_intrinsic_mut_keys_contains(
    contract: &Contract,
    handler: &Handler,
    name: &Ident,
    args: &[ExprKey],
    span: &Span,
) -> Inference {
    // This intrinsic expects exactly 3 arguments
    if args.len() != 1 {
        handler.emit_err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 1,
                found: args.len(),
                span: span.clone(),
            },
        });
    } else {
        // Helper lambda to emit arg type errors
        let arg_type_error = |expected, found, intrinsic_span, arg_span| Error::Compile {
            error: CompileError::MismatchedIntrinsicArgType {
                expected,
                found,
                intrinsic_span,
                arg_span,
            },
        };

        // The only argument is the mutable key which must be an array of integers
        let mut_key_span = args[0].get(contract).span();
        let mut_key_type = &args[0].get_ty(contract);
        if let Some(ty) = mut_key_type.get_array_el_type() {
            if !ty.is_int() {
                handler.emit_err(arg_type_error(
                    "int[..]".to_string(),
                    contract.with_ctrct(mut_key_type).to_string(),
                    name.span.clone(),
                    mut_key_span.clone(),
                ));
            }
        } else {
            handler.emit_err(arg_type_error(
                "int[..]".to_string(),
                contract.with_ctrct(mut_key_type).to_string(),
                name.span.clone(),
                mut_key_span.clone(),
            ));
        }
    }

    // This intrinsic returns a `bool`
    Inference::Type(Type::Primitive {
        kind: PrimitiveKind::Bool,
        span: span.clone(),
    })
}

//
// Intrinsic `__this_set_address`
//
// - Arguments: none
//
// - Returns: `b256`
//
// Description: Get the content hash of the contract that this predicate belongs to.
//
fn infer_intrinsic_this_set_address(handler: &Handler, args: &[ExprKey], span: &Span) -> Inference {
    // This intrinsic expects no arguments
    if !args.is_empty() {
        handler.emit_err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 0,
                found: args.len(),
                span: span.clone(),
            },
        });
    }

    // This intrinsic returns a `b256`
    Inference::Type(Type::Primitive {
        kind: PrimitiveKind::B256,
        span: span.clone(),
    })
}

//
// Intrinsic `__this_address`
//
// - Arguments: none
//
// - Returns: `b256`
//
// Description: Get the content hash of this predicate.
//
fn infer_intrinsic_this_address(handler: &Handler, args: &[ExprKey], span: &Span) -> Inference {
    // This intrinsic expects no arguments
    if !args.is_empty() {
        handler.emit_err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 0,
                found: args.len(),
                span: span.clone(),
            },
        });
    }

    // This intrinsic returns a `b256`
    Inference::Type(Type::Primitive {
        kind: PrimitiveKind::B256,
        span: span.clone(),
    })
}

//
// Intrinsic `__this_pathway`
//
// - Arguments: none
//
// - Returns: `int`
//
// Description: This operation returns the index of the solution data currently being used to check
// this predicate.
//
fn infer_intrinsic_this_pathway(handler: &Handler, args: &[ExprKey], span: &Span) -> Inference {
    // This intrinsic expects no arguments
    if !args.is_empty() {
        handler.emit_err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 0,
                found: args.len(),
                span: span.clone(),
            },
        });
    }

    // This intrinsic returns a `b256`
    Inference::Type(Type::Primitive {
        kind: PrimitiveKind::Int,
        span: span.clone(),
    })
}

//
// Intrinsic `__sha256`
//
// - Arguments:
//     * data: any type.
//
// - Returns: `b256`
//
// Description: Produce a SHA 256 hash from the specified data.
//
fn infer_intrinsic_sha256(handler: &Handler, args: &[ExprKey], span: &Span) -> Inference {
    // This intrinsic expects exactly 1 argument
    if args.len() != 1 {
        handler.emit_err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 1,
                found: args.len(),
                span: span.clone(),
            },
        });
    }

    // This intrinsic returns a `b256`
    Inference::Type(Type::Primitive {
        kind: PrimitiveKind::B256,
        span: span.clone(),
    })
}

//
// Intrinsic `__verify_ed25519`
//
// - Arguments:
//     * Data: any type.
//     * Signature: `{ b256, b256 }`
//     * Public key: `b256`
//
// - Returns: `bool`
//
// Description: Validate an Ed25519 signature against a public key.
//
fn infer_intrinsic_verify_ed25519(
    contract: &Contract,
    handler: &Handler,
    name: &Ident,
    args: &[ExprKey],
    span: &Span,
) -> Inference {
    // This intrinsic expects exactly 3 arguments
    if args.len() != 3 {
        handler.emit_err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 3,
                found: args.len(),
                span: span.clone(),
            },
        });
    } else {
        // Helper lambda to emit arg type errors
        let arg_type_error = |expected, found, intrinsic_span, arg_span| Error::Compile {
            error: CompileError::MismatchedIntrinsicArgType {
                expected,
                found,
                intrinsic_span,
                arg_span,
            },
        };

        // First argument is the data which can be anything so nothing to check

        // Second argument is the signature and must be a `{ b256, b256 }`
        let sig_span = args[1].get(contract).span();
        let sig_type = &args[1].get_ty(contract);
        if let Some(fields) = sig_type.get_tuple_fields() {
            if fields.len() != 2 || !fields[0].1.is_b256() || !fields[1].1.is_b256() {
                handler.emit_err(arg_type_error(
                    "{ b256, b256 }".to_string(),
                    contract.with_ctrct(sig_type).to_string(),
                    name.span.clone(),
                    sig_span.clone(),
                ));
            }
        } else {
            handler.emit_err(arg_type_error(
                "{ b256, b256 }".to_string(),
                contract.with_ctrct(sig_type).to_string(),
                name.span.clone(),
                sig_span.clone(),
            ));
        }

        // Third argument is the public key and must be a `b256`
        let pub_key_span = args[2].get(contract).span();
        let pub_key_type = &args[2].get_ty(contract);
        if !pub_key_type.is_b256() {
            handler.emit_err(arg_type_error(
                "b256".to_string(),
                contract.with_ctrct(pub_key_type).to_string(),
                name.span.clone(),
                pub_key_span.clone(),
            ));
        }
    }

    // This intrinsic returns a `bool`
    Inference::Type(Type::Primitive {
        kind: PrimitiveKind::Bool,
        span: span.clone(),
    })
}

//
// Intrinsic `__recover_secp256k1`
//
// - Arguments:
//     * Data hash: `b256`
//     * Signature: `{ b256, b256, int }`
//
// - Returns: `{ b256, int }`
//
// Description: Recover the public key from a secp256k1 signature.
//
fn infer_intrinsic_recover_secp256k1(
    contract: &Contract,
    handler: &Handler,
    name: &Ident,
    args: &[ExprKey],
    span: &Span,
) -> Inference {
    // This intrinsic expects exactly 2 arguments
    if args.len() != 2 {
        handler.emit_err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 2,
                found: args.len(),
                span: span.clone(),
            },
        });
    } else {
        // Helper lambda to emit arg type errors
        let arg_type_error = |expected, found, intrinsic_span, arg_span| Error::Compile {
            error: CompileError::MismatchedIntrinsicArgType {
                expected,
                found,
                intrinsic_span,
                arg_span,
            },
        };

        // First argument is the hash of the data and must be a `b256`
        let pub_key_span = args[0].get(contract).span();
        let pub_key_type = &args[0].get_ty(contract);
        if !pub_key_type.is_b256() {
            handler.emit_err(arg_type_error(
                "b256".to_string(),
                contract.with_ctrct(pub_key_type).to_string(),
                name.span.clone(),
                pub_key_span.clone(),
            ));
        }

        // Second argument is the signature and must be a `{ b256, b256, int }`
        let sig_span = args[1].get(contract).span();
        let sig_type = &args[1].get_ty(contract);
        if let Some(fields) = sig_type.get_tuple_fields() {
            if fields.len() != 3
                || !fields[0].1.is_b256()
                || !fields[1].1.is_b256()
                || !fields[2].1.is_int()
            {
                handler.emit_err(arg_type_error(
                    "{ b256, b256, int }".to_string(),
                    contract.with_ctrct(sig_type).to_string(),
                    name.span.clone(),
                    sig_span.clone(),
                ));
            }
        } else {
            handler.emit_err(arg_type_error(
                "{ b256, b256, int }".to_string(),
                contract.with_ctrct(sig_type).to_string(),
                name.span.clone(),
                sig_span.clone(),
            ));
        }
    }

    // This intrinsic returns the public key of type `{ b256, int }`
    Inference::Type(Type::Tuple {
        fields: vec![
            (
                None,
                Type::Primitive {
                    kind: PrimitiveKind::B256,
                    span: span.clone(),
                },
            ),
            (
                None,
                Type::Primitive {
                    kind: PrimitiveKind::Int,
                    span: span.clone(),
                },
            ),
        ],
        span: empty_span(),
    })
}

//
// Intrinsic `__state_len`
//
// - Arguments:
//     * State variable: any type
//
// - Returns: `int`
//
// Description: this operation returns the "length" of a state variable. The length of a state
// variable is the sum of the lengths of the slots that form the state variable.
//
fn infer_intrinsic_state_len(
    contract: &Contract,
    handler: &Handler,
    pred: Option<&Predicate>,
    args: &[ExprKey],
    span: &Span,
) -> Inference {
    // This intrinsic expects exactly 1 argument
    if args.len() != 1 {
        handler.emit_err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 1,
                found: args.len(),
                span: span.clone(),
            },
        });
    }

    // First argument must be a path to a state variable
    match args.first().and_then(|expr_key| expr_key.try_get(contract)) {
        Some(Expr::Path(name, _))
            if pred
                .map(|pred| pred.states().any(|(_, state)| state.name == *name))
                .unwrap_or(false) => {}
        Some(Expr::UnaryOp {
            op: UnaryOp::NextState,
            ..
        }) => {}
        None => {}
        _ => {
            handler.emit_err(Error::Compile {
                error: CompileError::IntrinsicArgMustBeStateVar { span: span.clone() },
            });
        }
    };

    // This intrinsic returns a `int`
    Inference::Type(Type::Primitive {
        kind: PrimitiveKind::Int,
        span: span.clone(),
    })
}

//
// Intrinsic `__vec_len`
//
// - Arguments:
//     * Storage access expression of type `Vector`
//
// - Returns: `int`
//
// Description: this operation returns the "length" of a storage vector variable.
//
fn infer_intrinsic_vec_len(
    contract: &Contract,
    handler: &Handler,
    pred: Option<&Predicate>,
    args: &[ExprKey],
    span: &Span,
) -> Inference {
    // This intrinsic expects exactly 1 argument
    if args.len() != 1 {
        handler.emit_err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 1,
                found: args.len(),
                span: span.clone(),
            },
        });
    }

    // The only argument must be a storage access of type vector
    match args.first().and_then(|expr_key| expr_key.try_get(contract)) {
        Some(Expr::StorageAccess(name, ..)) if contract.storage_var(name).1.ty.is_vector() => {}
        Some(Expr::ExternalStorageAccess {
            interface_instance,
            name,
            ..
        }) if pred
            .map(|pred| {
                contract
                    .external_storage_var(
                        &pred
                            .interface_instances
                            .iter()
                            .find(|e| e.name.to_string() == *interface_instance)
                            .expect("missing interface instance")
                            .interface,
                        name,
                    )
                    .1
                    .ty
                    .is_vector()
            })
            .unwrap_or(false) => {}
        None => {}
        _ => {
            handler.emit_err(Error::Compile {
                error: CompileError::IntrinsicArgMustBeStorageAccess { span: span.clone() },
            });
        }
    };

    // This intrinsic returns a `int`
    Inference::Type(Type::Primitive {
        kind: PrimitiveKind::Int,
        span: span.clone(),
    })
}
