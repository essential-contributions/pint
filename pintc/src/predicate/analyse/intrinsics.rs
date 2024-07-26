use super::Inference;
use super::{Contract, ExprKey, Ident, Predicate};
use crate::{
    error::{CompileError, Error},
    expr::{Expr, UnaryOp},
    span::{empty_span, Span, Spanned},
    types::{PrimitiveKind, Type},
};

impl Contract {
    pub(super) fn infer_intrinsic_call_expr(
        &self,
        pred: &Predicate,
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
                "__mut_keys_len" => infer_intrinsic_mut_keys_len(args, span),
                "__mut_keys_contains" => {
                    infer_intrinsic_mut_keys_contains(self, pred, name, args, span)
                }
                "__this_address" => infer_intrinsic_this_address(args, span),
                "__this_set_address" => infer_intrinsic_this_set_address(args, span),
                "__this_pathway" => infer_intrinsic_this_pathway(args, span),

                // Crypto ops
                "__sha256" => infer_intrinsic_sha256(args, span),
                "__verify_ed25519" => infer_intrinsic_verify_ed25519(self, pred, name, args, span),
                "__recover_secp256k1" => {
                    infer_intrinsic_recover_secp256k1(self, pred, name, args, span)
                }

                "__state_len" => infer_intrinsic_state_len(self, pred, args, span),

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
fn infer_intrinsic_mut_keys_len(args: &[ExprKey], span: &Span) -> Result<Inference, Error> {
    // This intrinsic expects no arguments
    if !args.is_empty() {
        return Err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 0,
                found: args.len(),
                span: span.clone(),
            },
        });
    }

    // This intrinsic returns a `int`
    Ok(Inference::Type(Type::Primitive {
        kind: PrimitiveKind::Int,
        span: span.clone(),
    }))
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
    pred: &Predicate,
    name: &Ident,
    args: &[ExprKey],
    span: &Span,
) -> Result<Inference, Error> {
    // This intrinsic expects exactly 3 arguments
    if args.len() != 1 {
        return Err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 1,
                found: args.len(),
                span: span.clone(),
            },
        });
    }

    // Helper lambda to emit arg type errors
    let arg_type_error = |expected, found, intrinsic_span, arg_span| {
        Err(Error::Compile {
            error: CompileError::MismatchedIntrinsicArgType {
                expected,
                found,
                intrinsic_span,
                arg_span,
            },
        })
    };

    // The only argument is the mutable key which must be an array of integers
    let mut_key_span = args[0].get(contract).span();
    let mut_key_type = &args[0].get_ty(contract);
    if let Some(ty) = mut_key_type.get_array_el_type() {
        if !ty.is_int() {
            return arg_type_error(
                "int[..]".to_string(),
                pred.with_pred(contract, mut_key_type).to_string(),
                name.span.clone(),
                mut_key_span.clone(),
            );
        }
    } else {
        return arg_type_error(
            "int[..]".to_string(),
            pred.with_pred(contract, mut_key_type).to_string(),
            name.span.clone(),
            mut_key_span.clone(),
        );
    }

    // This intrinsic returns a `bool`
    Ok(Inference::Type(Type::Primitive {
        kind: PrimitiveKind::Bool,
        span: span.clone(),
    }))
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
fn infer_intrinsic_this_set_address(args: &[ExprKey], span: &Span) -> Result<Inference, Error> {
    // This intrinsic expects no arguments
    if !args.is_empty() {
        return Err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 0,
                found: args.len(),
                span: span.clone(),
            },
        });
    }

    // This intrinsic returns a `b256`
    Ok(Inference::Type(Type::Primitive {
        kind: PrimitiveKind::B256,
        span: span.clone(),
    }))
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
fn infer_intrinsic_this_address(args: &[ExprKey], span: &Span) -> Result<Inference, Error> {
    // This intrinsic expects no arguments
    if !args.is_empty() {
        return Err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 0,
                found: args.len(),
                span: span.clone(),
            },
        });
    }

    // This intrinsic returns a `b256`
    Ok(Inference::Type(Type::Primitive {
        kind: PrimitiveKind::B256,
        span: span.clone(),
    }))
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
fn infer_intrinsic_this_pathway(args: &[ExprKey], span: &Span) -> Result<Inference, Error> {
    // This intrinsic expects no arguments
    if !args.is_empty() {
        return Err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 0,
                found: args.len(),
                span: span.clone(),
            },
        });
    }

    // This intrinsic returns a `b256`
    Ok(Inference::Type(Type::Primitive {
        kind: PrimitiveKind::Int,
        span: span.clone(),
    }))
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
fn infer_intrinsic_sha256(args: &[ExprKey], span: &Span) -> Result<Inference, Error> {
    // This intrinsic expects exactly 1 argument
    if args.len() != 1 {
        return Err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 1,
                found: args.len(),
                span: span.clone(),
            },
        });
    }

    // This intrinsic returns a `b256`
    Ok(Inference::Type(Type::Primitive {
        kind: PrimitiveKind::B256,
        span: span.clone(),
    }))
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
    pred: &Predicate,
    name: &Ident,
    args: &[ExprKey],
    span: &Span,
) -> Result<Inference, Error> {
    // This intrinsic expects exactly 3 arguments
    if args.len() != 3 {
        return Err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 3,
                found: args.len(),
                span: span.clone(),
            },
        });
    }

    // Helper lambda to emit arg type errors
    let arg_type_error = |expected, found, intrinsic_span, arg_span| {
        Err(Error::Compile {
            error: CompileError::MismatchedIntrinsicArgType {
                expected,
                found,
                intrinsic_span,
                arg_span,
            },
        })
    };

    // First argument is the data which can be anything so nothing to check

    // Second argument is the signature and must be a `{ b256, b256 }`
    let sig_span = args[1].get(contract).span();
    let sig_type = &args[1].get_ty(contract);
    if let Some(fields) = sig_type.get_tuple_fields() {
        if fields.len() != 2 || !fields[0].1.is_b256() || !fields[1].1.is_b256() {
            return arg_type_error(
                "{ b256, b256 }".to_string(),
                pred.with_pred(contract, sig_type).to_string(),
                name.span.clone(),
                sig_span.clone(),
            );
        }
    } else {
        return arg_type_error(
            "{ b256, b256 }".to_string(),
            pred.with_pred(contract, sig_type).to_string(),
            name.span.clone(),
            sig_span.clone(),
        );
    }

    // Third argument is the public key and must be a `b256`
    let pub_key_span = args[2].get(contract).span();
    let pub_key_type = &args[2].get_ty(contract);
    if !pub_key_type.is_b256() {
        return arg_type_error(
            "b256".to_string(),
            pred.with_pred(contract, pub_key_type).to_string(),
            name.span.clone(),
            pub_key_span.clone(),
        );
    }

    // This intrinsic returns a `bool`
    Ok(Inference::Type(Type::Primitive {
        kind: PrimitiveKind::Bool,
        span: span.clone(),
    }))
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
    pred: &Predicate,
    name: &Ident,
    args: &[ExprKey],
    span: &Span,
) -> Result<Inference, Error> {
    // This intrinsic expects exactly 2 arguments
    if args.len() != 2 {
        return Err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 2,
                found: args.len(),
                span: span.clone(),
            },
        });
    }

    // Helper lambda to emit arg type errors
    let arg_type_error = |expected, found, intrinsic_span, arg_span| {
        Err(Error::Compile {
            error: CompileError::MismatchedIntrinsicArgType {
                expected,
                found,
                intrinsic_span,
                arg_span,
            },
        })
    };

    // First argument is the hash of the data and must be a `b256`
    let pub_key_span = args[0].get(contract).span();
    let pub_key_type = &args[0].get_ty(contract);
    if !pub_key_type.is_b256() {
        return arg_type_error(
            "b256".to_string(),
            pred.with_pred(contract, pub_key_type).to_string(),
            name.span.clone(),
            pub_key_span.clone(),
        );
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
            return arg_type_error(
                "{ b256, b256, int }".to_string(),
                pred.with_pred(contract, sig_type).to_string(),
                name.span.clone(),
                sig_span.clone(),
            );
        }
    } else {
        return arg_type_error(
            "{ b256, b256, int }".to_string(),
            pred.with_pred(contract, sig_type).to_string(),
            name.span.clone(),
            sig_span.clone(),
        );
    }

    // This intrinsic returns the public key of type `{ b256, int }`
    Ok(Inference::Type(Type::Tuple {
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
    }))
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
    pred: &Predicate,
    args: &[ExprKey],
    span: &Span,
) -> Result<Inference, Error> {
    // This intrinsic expects exactly 1 argument
    if args.len() != 1 {
        return Err(Error::Compile {
            error: CompileError::UnexpectedIntrinsicArgCount {
                expected: 1,
                found: args.len(),
                span: span.clone(),
            },
        });
    }

    // First argument must be a path to a state variable
    match args[0].try_get(contract) {
        Some(Expr::Path(name, _)) if pred.states().any(|(_, state)| state.name == *name) => Ok(()),
        Some(Expr::UnaryOp {
            op: UnaryOp::NextState,
            ..
        }) => Ok(()),
        _ => Err(Error::Compile {
            error: CompileError::IntrinsicArgMustBeStateVar { span: span.clone() },
        }),
    }?;

    // This intrinsic returns a `int`
    Ok(Inference::Type(Type::Primitive {
        kind: PrimitiveKind::Int,
        span: span.clone(),
    }))
}
