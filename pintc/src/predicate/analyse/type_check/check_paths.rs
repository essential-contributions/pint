use super::Inference;
use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    predicate::{Const, Contract, Ident, Predicate},
    span::Span,
    types::{EphemeralDecl, NewTypeDecl, Type, UnionDecl},
};

impl Contract {
    pub(super) fn infer_path_by_name(
        &self,
        handler: &Handler,
        pred: Option<&Predicate>,
        path: &String,
        span: &Span,
    ) -> Inference {
        // If we're searching for a union variant and it appears to be unqualified then we can
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
            // TODO: What is this matching?  When would an expression just be an alias?
            // It's a fully matched newtype.
            Inference::Type(ty.clone())
        } else {
            // It might be a union variant. If it isn't we get a handy list of potential
            // variant names we can return in our error.
            let union_res = self.infer_variant_by_name(handler, path, span);
            if let Ok(inference) = union_res {
                // Need to translate the type between Results.
                inference
            } else {
                // Save the unions variants list for the SymbolNotFound error if we need it.
                if let Ok(unions_list) = union_res.unwrap_err() {
                    hints.extend(unions_list);
                }

                // For all other paths we need a predicate.
                if let Some(pred) = pred {
                    if let Some(param_ty) = pred
                        .params
                        .iter()
                        .find_map(|param| (&param.name.name == path).then_some(param.ty.clone()))
                    {
                        Inference::Type(param_ty.clone())
                    } else if let Some((variable_key, variable)) = pred
                        .variables()
                        .find(|(_, variable)| (&variable.name == path))
                    {
                        // It's variable.
                        let variable_expr_ty = variable.expr.get_ty(self);
                        let variable_type = variable_key.get_ty(pred);
                        if !variable_type.is_unknown() {
                            Inference::Type(variable_type.clone())
                        } else if !variable_expr_ty.is_unknown() {
                            Inference::Type(variable_expr_ty.clone())
                        } else {
                            Inference::Dependant(variable.expr)
                        }
                    } else if let Some(EphemeralDecl { ty, .. }) = pred
                        .ephemerals
                        .iter()
                        .find(|eph_decl| &eph_decl.name == path)
                    {
                        // It's an ephemeral value.
                        Inference::Type(ty.clone())
                    } else {
                        // None of the above.
                        handler.emit_err(Error::Compile {
                            error: CompileError::SymbolNotFound {
                                name: path.clone(),
                                span: span.clone(),
                                union_names: hints,
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
        path: &String,
        span: &Span,
    ) -> Result<Inference, Result<Vec<String>, ErrorEmitted>> {
        // Check first if the path prefix matches a new type.
        for NewTypeDecl { name, ty, .. } in &self.new_types {
            if let Type::Union { decl, .. } = ty {
                // This new type is to a union.  Does the new type path match the passed path?
                if path.starts_with(&name.name) {
                    // Yep, we might have an union wrapped in a new type.
                    let new_type_len = name.name.len();
                    if path.chars().nth(new_type_len) == Some(':') {
                        // Definitely worth trying.  Recurse.
                        let union_name = &self.unions[*decl].name;
                        let new_path = union_name.name.clone() + &path[new_type_len..];
                        if let ty @ Ok(_) = self.infer_variant_by_name(handler, &new_path, span) {
                            // We found a union variant.
                            return ty;
                        }
                    }
                }
            }
        }

        self.infer_union_variant_by_name(handler, path, span, Vec::new())
    }

    // This has the same tricky Result<_, Result<_, _>> return type -- see infer_variant_by_name()
    // above.
    fn infer_union_variant_by_name(
        &self,
        handler: &Handler,
        path: &String,
        path_span: &Span,
        mut hints: Vec<String>,
    ) -> Result<Inference, Result<Vec<String>, ErrorEmitted>> {
        // Try to find a match in the unions.
        let variant_match: Option<Result<Inference, ErrorEmitted>> = self.unions.iter().find_map(
            |(
                decl,
                UnionDecl {
                    name: union_name,
                    variants,
                    span,
                },
            )| {
                if &union_name.name == path {
                    Some(Ok(Inference::Type(Type::Union {
                        decl,
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
                            // this point we have received only a String with no value.)
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
                                    decl,
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
        path: &String,
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
