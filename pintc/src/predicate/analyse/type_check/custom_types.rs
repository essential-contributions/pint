use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    predicate::{Const, Contract, Expr, Interface, InterfaceVar, StorageVar},
    span::{empty_span, Span},
    types::{NewTypeDecl, Type, UnionDecl},
};

use fxhash::FxHashMap;

impl Contract {
    /// Lower every `Type::Custom` type in a contract to a `Type::Alias` type if the custom type
    /// actually matches one of the new type declarations in the contract.
    pub(in crate::predicate::analyse) fn lower_custom_types(
        &mut self,
        handler: &Handler,
    ) -> Result<(), ErrorEmitted> {
        self.check_recursive_newtypes(handler)?;
        self.lower_custom_types_in_newtypes(handler)?;
        self.lower_custom_types_in_contract();

        Ok(())
    }

    fn check_recursive_newtypes(&self, handler: &Handler) -> Result<(), ErrorEmitted> {
        fn inspect_type_names<'a>(
            handler: &Handler,
            contract: &'a Contract,
            seen_names: &mut FxHashMap<&'a str, &'a Span>,
            ty: &'a Type,
        ) -> Result<(), ErrorEmitted> {
            match ty {
                Type::Array { ty, .. } => inspect_type_names(handler, contract, seen_names, ty),

                Type::Tuple { fields, .. } => fields
                    .iter()
                    .try_for_each(|(_, ty)| inspect_type_names(handler, contract, seen_names, ty)),

                Type::Union { path, .. } => {
                    // This was a custom type which has been confirmed to be a union.
                    let Some(union_decl) = contract.unions.iter().find(|ud| &ud.name.name == path)
                    else {
                        unreachable!("union type with unknown path");
                    };

                    for variant in &union_decl.variants {
                        if let Some(ty) = &variant.ty {
                            inspect_type_names(handler, contract, seen_names, ty)?;
                        }
                    }

                    Ok(())
                }

                Type::Custom {
                    path,
                    span: custom_ty_span,
                } => {
                    // Look-up the name to confirm it's a new-type.
                    if let Some((new_ty, new_ty_span)) =
                        contract
                            .new_types
                            .iter()
                            .find_map(|NewTypeDecl { name, ty, span }| {
                                (path == &name.name).then_some((ty, span))
                            })
                    {
                        // This is a new-type; have we seen it before?
                        if let Some(seen_span) = seen_names.get(path.as_str()) {
                            // We have!  Bad news.
                            Err(handler.emit_err(Error::Compile {
                                error: CompileError::RecursiveNewType {
                                    name: path.to_string(),
                                    decl_span: (*seen_span).clone(),
                                    use_span: custom_ty_span.clone(),
                                },
                            }))
                        } else {
                            // We need to add then remove the new path to the 'seen' list; if we
                            // don't remove it we'll get false positives.
                            seen_names.insert(path.as_str(), new_ty_span);
                            let res = inspect_type_names(handler, contract, seen_names, new_ty);
                            seen_names.remove(path.as_str());
                            res
                        }
                    } else {
                        // Will be a path to an enum or variant.
                        Ok(())
                    }
                }

                Type::Alias { ty, .. } => inspect_type_names(handler, contract, seen_names, ty),

                Type::Map { ty_from, ty_to, .. } => {
                    inspect_type_names(handler, contract, seen_names, ty_from)?;
                    inspect_type_names(handler, contract, seen_names, ty_to)
                }

                Type::Vector { ty, .. } => inspect_type_names(handler, contract, seen_names, ty),

                Type::Error(_) | Type::Unknown(_) | Type::Any(_) | Type::Primitive { .. } => Ok(()),
            }
        }

        for NewTypeDecl { name, ty, span } in &self.new_types {
            let mut seen_names = FxHashMap::from_iter(std::iter::once((name.name.as_str(), span)));

            let _ = inspect_type_names(handler, self, &mut seen_names, ty);
        }

        handler.result(())
    }

    /// Lower every `Type::Custom` type in a new type declarations to a `Type::Alias` type if the
    /// custom type actually matches one of the new type declarations in the contract. All
    /// remaining custom types after this method runs should refer to enums.
    fn lower_custom_types_in_newtypes(&mut self, handler: &Handler) -> Result<(), ErrorEmitted> {
        // Search for a custom type with a specific name and return a mut ref to it.
        fn get_custom_type_mut_ref<'a>(
            custom_path: &str,
            ty: &'a mut Type,
        ) -> Option<&'a mut Type> {
            match ty {
                Type::Array { ty, .. } => get_custom_type_mut_ref(custom_path, ty),
                Type::Tuple { fields, .. } => fields
                    .iter_mut()
                    .find_map(|(_, fld_ty)| get_custom_type_mut_ref(custom_path, fld_ty)),
                Type::Custom { path, .. } => (path == custom_path).then_some(ty),
                Type::Alias { ty, .. } => get_custom_type_mut_ref(custom_path, ty),
                Type::Map { ty_from, ty_to, .. } => get_custom_type_mut_ref(custom_path, ty_from)
                    .or_else(|| get_custom_type_mut_ref(custom_path, ty_to)),
                Type::Vector { ty, .. } => get_custom_type_mut_ref(custom_path, ty),
                Type::Error(_)
                | Type::Unknown(_)
                | Type::Any(_)
                | Type::Primitive { .. }
                | Type::Union { .. } => None,
            }
        }

        // Any custom types referred to *within new types* need to be converted to type aliases.
        // E.g.,
        //   type A = int;
        //   type B = { A, A };
        //   // B will have `Type::Custom("A")` which need to be `Type::Alias("A", int)`

        for new_type_idx in 0..self.new_types.len() {
            // We're replacing only a single new type at a time, if found in other new-types.
            let new_type = self.new_types[new_type_idx].clone();

            // Replace the next found custom type which needs to be replaced with a an alias.
            // There may be multiple replacements required per iteration, so we'll visit every
            // current new-type decl per iteration until none are updated.
            for loop_check in 0.. {
                let mut modified = false;

                for NewTypeDecl { ref mut ty, .. } in &mut self.new_types {
                    if let Some(custom_ty) = get_custom_type_mut_ref(&new_type.name.name, ty) {
                        *custom_ty = Type::Alias {
                            path: new_type.name.name.clone(),
                            ty: Box::new(new_type.ty.clone()),
                            span: new_type.span.clone(),
                        };

                        modified = true;
                    }
                }

                if !modified {
                    break;
                }

                if loop_check > 10_000 {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "infinite loop in lower_custom_types_in_newtypes()",
                            span: empty_span(),
                        },
                    }));
                }
            }
        }

        Ok(())
    }

    /// Lower every `Type::Custom` type in a contract to a `Type::Alias` type if the custom type
    /// actually matches one of the new type declarations in the contract. All remaining custom
    /// types after this method runs should refer to enums.
    fn lower_custom_types_in_contract(&mut self) {
        use std::borrow::BorrowMut;

        // Given a mutable reference to a `Type` and a list of new type declarations `new_types`,
        // replace it or its subtypes with a `Type::Alias` when a `Type::Custom` is encountered
        // that matches the name of a declaration in `new_types`.
        fn replace_custom_type(new_types: &[NewTypeDecl], union_names: &[String], ty: &mut Type) {
            match ty {
                Type::Array { ty, .. } => {
                    replace_custom_type(new_types, union_names, ty.borrow_mut())
                }
                Type::Tuple { fields, .. } => fields
                    .iter_mut()
                    .for_each(|(_, ty)| replace_custom_type(new_types, union_names, ty)),
                Type::Custom { path, span } => {
                    let path = path.clone();
                    if let Some((new_ty, new_span)) =
                        new_types.iter().find_map(|NewTypeDecl { name, ty, span }| {
                            (name.name == path).then_some((ty, span))
                        })
                    {
                        *ty = Type::Alias {
                            path,
                            ty: Box::new(new_ty.clone()),
                            span: new_span.clone(),
                        };
                    } else if union_names.iter().any(|name| name == &path) {
                        *ty = Type::Union {
                            path,
                            span: span.clone(),
                        }
                    }
                }
                Type::Alias { ty, .. } => replace_custom_type(new_types, union_names, ty),
                Type::Map { ty_from, ty_to, .. } => {
                    replace_custom_type(new_types, union_names, ty_from);
                    replace_custom_type(new_types, union_names, ty_to);
                }
                Type::Vector { ty, .. } => replace_custom_type(new_types, union_names, ty),
                Type::Error(_)
                | Type::Unknown(_)
                | Type::Any(_)
                | Type::Primitive { .. }
                | Type::Union { .. } => {}
            }
        }

        let union_names = self
            .unions
            .iter()
            .map(|UnionDecl { name, .. }| name.name.clone())
            .collect::<Vec<_>>();

        for UnionDecl { variants, .. } in &mut self.unions {
            for variant in variants {
                if let Some(variant_ty) = &mut variant.ty {
                    replace_custom_type(&self.new_types, &union_names, variant_ty);
                }
            }
        }

        // Replace custom types in predicates
        self.preds.values_mut().for_each(|pred| {
            // Replace custom types in vars and states
            pred.vars
                .update_types(|_, ty| replace_custom_type(&self.new_types, &union_names, ty));
            pred.states
                .update_types(|_, ty| replace_custom_type(&self.new_types, &union_names, ty));

            // Replace custom types in any `as` cast expression
            self.exprs.update_exprs(|_, expr| {
                if let Expr::Cast { ty, .. } = expr {
                    replace_custom_type(&self.new_types, &union_names, ty.borrow_mut());
                }
            });
        });

        // Replace custom types in `const` declarations
        self.consts.values_mut().for_each(|Const { decl_ty, .. }| {
            replace_custom_type(&self.new_types, &union_names, decl_ty)
        });

        // Replace custom types in every storage variable in the contract
        if let Some((storage_vars, _)) = self.storage.as_mut() {
            storage_vars.iter_mut().for_each(|StorageVar { ty, .. }| {
                replace_custom_type(&self.new_types, &union_names, ty);
            })
        }

        // Replace custom types in interfaces
        self.interfaces.iter_mut().for_each(
            |Interface {
                 storage,
                 predicate_interfaces,
                 ..
             }| {
                // Replace custom types in every storage variable in the interface
                if let Some((storage_vars, _)) = storage.as_mut() {
                    storage_vars.iter_mut().for_each(|StorageVar { ty, .. }| {
                        replace_custom_type(&self.new_types, &union_names, ty);
                    });
                }

                // Replace custom types in every decision variable in the interface. These belong
                // to the various predicate interfaces
                predicate_interfaces
                    .iter_mut()
                    .for_each(|predicate_interface| {
                        predicate_interface.vars.iter_mut().for_each(
                            |InterfaceVar { ty, .. }| {
                                replace_custom_type(&self.new_types, &union_names, ty)
                            },
                        );
                    });
            },
        );
    }
}
