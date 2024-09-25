use super::Inference;
use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    predicate::{Const, Contract, Ident, Predicate, VarKey},
    span::Span,
    types::{EnumDecl, EphemeralDecl, NewTypeDecl, Path, Type, UnionDecl},
};

impl Contract {
    fn infer_path_by_key(
        &self,
        handler: &Handler,
        pred: &Predicate,
        var_key: VarKey,
        span: &Span,
    ) -> Inference {
        let ty = var_key.get_ty(pred);
        if !ty.is_unknown() {
            Inference::Type(ty.clone())
        } else if let Some(init_expr_key) = pred.var_inits.get(var_key) {
            let init_expr_ty = init_expr_key.get_ty(self);
            if !init_expr_ty.is_unknown() {
                Inference::Type(init_expr_ty.clone())
            } else {
                // We have a variable with an initialiser but don't know the initialiser type
                // yet.
                Inference::Dependant(*init_expr_key)
            }
        } else {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "untyped variable doesn't have initialiser",
                    span: span.clone(),
                },
            });
            Inference::Type(Type::Error(span.clone()))
        }
    }

    pub(super) fn infer_path_by_name(
        &self,
        handler: &Handler,
        pred: Option<&Predicate>,
        path: &Path,
        span: &Span,
    ) -> Inference {
        // If we're searching for an enum variant and it appears to be unqualified then we can
        // report some hints.
        let mut hints = Vec::new();

        if let Some(Const { decl_ty, .. }) = self.consts.get(path) {
            if !decl_ty.is_unknown() {
                Inference::Type(decl_ty.clone())
            } else {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "const decl has unknown type *after* evaluation",
                        span: span.clone(),
                    },
                });
                Inference::Type(Type::Error(span.clone()))
            }
        } else if let Some(ty) = self
            .new_types
            .iter()
            .find_map(|NewTypeDecl { name, ty, .. }| (&name.name == path).then_some(ty))
        {
            // It's a fully matched newtype.
            Inference::Type(ty.clone())
        } else {
            // It might be an enum or union variant.  If it isn't we get a handy list of potential
            // variant names we can return in our error.
            let enum_res = self.infer_variant_by_name(handler, path, span);
            if let Ok(inference) = enum_res {
                // Need to translate the type between Results.
                inference
            } else {
                // Save the enums variants list for the SymbolNotFound error if we need it.
                if let Ok(enums_list) = enum_res.unwrap_err() {
                    hints.extend(enums_list);
                }

                // For all other paths we need a predicate.
                if let Some(pred) = pred {
                    if let Some(var_key) = pred
                        .vars()
                        .find_map(|(var_key, var)| (&var.name == path).then_some(var_key))
                    {
                        // It's a var.
                        self.infer_path_by_key(handler, pred, var_key, span)
                    } else if let Some((state_key, state)) =
                        pred.states().find(|(_, state)| (&state.name == path))
                    {
                        // It's state.
                        let state_expr_ty = state.expr.get_ty(self);
                        let state_type = state_key.get_ty(pred);
                        if !state_type.is_unknown() {
                            Inference::Type(state_type.clone())
                        } else if !state_expr_ty.is_unknown() {
                            Inference::Type(state_expr_ty.clone())
                        } else {
                            Inference::Dependant(state.expr)
                        }
                    } else if let Some(EphemeralDecl { ty, .. }) = pred
                        .ephemerals
                        .iter()
                        .find(|eph_decl| &eph_decl.name == path)
                    {
                        // It's an ephemeral value.
                        Inference::Type(ty.clone())
                    } else if let Some(ty) = self.infer_extern_var(pred, path) {
                        // It's an external var
                        ty
                    } else {
                        // None of the above.
                        handler.emit_err(Error::Compile {
                            error: CompileError::SymbolNotFound {
                                name: path.clone(),
                                span: span.clone(),
                                enum_names: hints,
                            },
                        });
                        Inference::Type(Type::Error(span.clone()))
                    }
                } else {
                    handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "attempting to infer item without required predicate ref",
                            span: span.clone(),
                        },
                    });
                    Inference::Type(Type::Error(span.clone()))
                }
            }
        }
    }

    // This has a wacky return type of Result<_, Result<_, _>>.  It's because it wants to return 3
    // different outcomes - an inference, a type error or some hints to use in a different error.
    // An alternative might be to use Result<Result<_, _>, _> or probably better would be
    // Result<Either<_, _>, _>.
    pub(super) fn infer_variant_by_name(
        &self,
        handler: &Handler,
        path: &Path,
        span: &Span,
    ) -> Result<Inference, Result<Vec<String>, ErrorEmitted>> {
        // Check first if the path prefix matches a new type.
        for NewTypeDecl { name, ty, .. } in &self.new_types {
            if let Type::Custom {
                path: enum_or_union_path,
                ..
            } = ty
            {
                // This new type is to an enum or union.  Does the new type path match the passed
                // path?
                if path.starts_with(&name.name) {
                    // Yep, we might have an enum or union wrapped in a new type.
                    let new_type_len = name.name.len();
                    if path.chars().nth(new_type_len) == Some(':') {
                        // Definitely worth trying.  Recurse.
                        let new_path = enum_or_union_path.clone() + &path[new_type_len..];
                        if let ty @ Ok(_) = self.infer_variant_by_name(handler, &new_path, span) {
                            // We found an enum or union variant.
                            return ty;
                        }
                    }
                }
            }
        }

        match self.infer_enum_variant_by_name(path) {
            // No dice.  Try unions, passing the hints along.
            Err(hints) => self.infer_union_variant_by_name(handler, path, span, hints),

            // Wrap the hints in a Result.
            inference => inference.map_err(Ok),
        }
    }

    pub(in crate::predicate::analyse) fn infer_enum_variant_by_name(
        &self,
        path: &Path,
    ) -> Result<Inference, Vec<String>> {
        let mut hints = Vec::default();

        // Find a match in the enums.
        self.enums
            .iter()
            .find_map(
                |EnumDecl {
                     name: enum_name,
                     variants,
                     span,
                 }| {
                    (&enum_name.name == path
                        || variants.iter().any(|variant| {
                            Self::variant_name_matches(path, enum_name, &variant.name, &mut hints)
                        }))
                    .then(|| {
                        Inference::Type(Type::Custom {
                            path: enum_name.name.clone(),
                            span: span.clone(),
                        })
                    })
                },
            )
            .ok_or(hints)
    }

    // This has the same tricky Result<_, Result<_, _>> return type -- see infer_variant_by_name()
    // above.
    fn infer_union_variant_by_name(
        &self,
        handler: &Handler,
        path: &Path,
        path_span: &Span,
        mut hints: Vec<String>,
    ) -> Result<Inference, Result<Vec<String>, ErrorEmitted>> {
        // Try to find a match in the unions.
        let variant_match: Option<Result<Inference, ErrorEmitted>> = self.unions.iter().find_map(
            |UnionDecl {
                 name: union_name,
                 variants,
                 span,
             }| {
                if &union_name.name == path {
                    Some(Ok(Inference::Type(Type::Union {
                        path: union_name.name.clone(),
                        span: span.clone(),
                    })))
                } else {
                    // Return None if not found or Some(Result<..>) depending on if it's valid.
                    variants.iter().find_map(|variant| {
                        Self::variant_name_matches(
                            path,
                            union_name,
                            &variant.variant_name.name,
                            &mut hints,
                        )
                        .then(|| {
                            // A variant was found.  Was it supposed to have a value?  (To get to
                            // this point we have received only a Path with no value.)
                            if variant.ty.is_some() {
                                // This variant *does* require a value.
                                Err(handler.emit_err(Error::Compile {
                                    error: CompileError::MissingUnionExprValue {
                                        name: variant.variant_name.name.to_string(),
                                        variant_ty: self
                                            .with_ctrct(variant.ty.as_ref().unwrap())
                                            .to_string(),
                                        span: path_span.clone(),
                                    },
                                }))
                            } else {
                                Ok(Inference::Type(Type::Union {
                                    path: union_name.name.clone(),
                                    span: span.clone(),
                                }))
                            }
                        })
                    })
                }
            },
        );

        // Translate the result from an Option to Result.
        variant_match
            .map(|res| res.map_err(Err))
            .unwrap_or(Err(Ok(hints)))
    }

    fn variant_name_matches(
        path: &Path,
        type_name: &Ident,
        variant_name: &str,
        hints: &mut Vec<String>,
    ) -> bool {
        if path.len() > 2 && &path[2..] == variant_name {
            hints.push(type_name.name.clone());
        }

        let mut full_variant = type_name.name.clone();
        full_variant.push_str("::");
        full_variant.push_str(variant_name);

        &full_variant == path
    }
}
