use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    predicate::{Contract, UnionKey},
    span::{empty_span, Span},
    types::{NewTypeDecl, Type, UnionDecl},
};

use fxhash::FxHashMap;

impl Contract {
    /// Lower every `Type::Custom` type in a contract to either a `Type::Alias` or `Type::Union`.
    /// No custom types should be left anywhere in the contract afterwards.
    pub(in crate::predicate::analyse) fn lower_custom_types(
        &mut self,
        handler: &Handler,
    ) -> Result<(), ErrorEmitted> {
        self.check_recursive_newtypes(handler)?;
        self.lower_nested_newtypes(handler)?;
        self.lower_custom_types_in_contract();

        //self.lower_custom_types_in_newtypes(handler)?;

        Ok(())
    }

    // Replace any nested aliases with a direct alias, cutting out the middle-man.  E.g.,
    // type count = int;
    // type amount = count;
    // type quantity = amount;
    //   =>
    // type amount = int;
    // type quantity = int;
    fn lower_nested_newtypes(&mut self, handler: &Handler) -> Result<(), ErrorEmitted> {
        fn replace_alias_target(
            alias_map: &fxhash::FxHashMap<String, Type>,
            target_ty: &mut Type,
        ) -> bool {
            let mut modified = false;

            match target_ty {
                Type::Custom { name, .. } => {
                    // If it's know then replace it.  We leave unknown custom types alone and
                    // they'll get picked up later in the type checker.
                    if let Some(new_ty) = alias_map.get(name) {
                        *target_ty = new_ty.clone();
                        modified = true;
                    }
                }

                Type::Alias { ty, .. } => {
                    // Replace the target type with this unwrapped type.
                    *target_ty = ty.as_ref().clone();
                    modified = true;
                }

                // Recurse.
                Type::Array { ty, .. } => {
                    modified = replace_alias_target(alias_map, ty);
                }
                Type::Tuple { fields, .. } => {
                    // We need a .try_any() here.
                    modified = fields.iter_mut().fold(false, |acc, (_, ty)| {
                        if acc {
                            acc
                        } else {
                            replace_alias_target(alias_map, ty)
                        }
                    });
                }
                Type::Map { ty_from, ty_to, .. } => {
                    modified = replace_alias_target(alias_map, ty_from)
                        || replace_alias_target(alias_map, ty_to);
                }
                Type::Vector { ty, .. } => {
                    modified = replace_alias_target(alias_map, ty);
                }

                // We handle unions separately.
                Type::Union { .. } => {}

                // Ignore.
                Type::Error(_) | Type::Unknown(_) | Type::Any(_) | Type::Primitive { .. } => {}
            }

            modified
        }

        for loop_check in 0.. {
            let mut modified = false;

            // Make a map of all named types, either aliases or unions.  This is a fairly expensive
            // cloning of a bunch of stuff, but generally it only happens once and it's actually used.
            let alias_map = fxhash::FxHashMap::from_iter(
                self.new_types
                    .iter()
                    .map(|NewTypeDecl { name, ty, .. }| (name.name.clone(), ty.clone()))
                    .chain(
                        self.unions
                            .iter()
                            .map(|(decl, UnionDecl { name, span, .. })| {
                                (
                                    name.name.clone(),
                                    Type::Union {
                                        decl,
                                        span: span.clone(),
                                    },
                                )
                            }),
                    ),
            );

            // Loop for all the current alias decls and replace the first nested type we find.
            for NewTypeDecl { ty, .. } in &mut self.new_types {
                if replace_alias_target(&alias_map, ty) {
                    modified = true;
                    break;
                }
            }

            if !modified {
                break;
            }

            if loop_check > 10_000 {
                return Err(handler.emit_internal_err(
                    "infinite loop in lower_nested_newtypes()".to_string(),
                    empty_span(),
                ));
            }
        }

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

                Type::Union { decl, .. } => {
                    let union_decl = &contract.unions[*decl];

                    for variant in &union_decl.variants {
                        if let Some(ty) = &variant.ty {
                            inspect_type_names(handler, contract, seen_names, ty)?;
                        }
                    }

                    Ok(())
                }

                Type::Custom {
                    name: custom_name,
                    span: custom_ty_span,
                } => {
                    // Look-up the name to confirm it's a new-type.
                    if let Some((new_ty, new_ty_span)) =
                        contract
                            .new_types
                            .iter()
                            .find_map(|NewTypeDecl { name, ty, span }| {
                                (custom_name == &name.name).then_some((ty, span))
                            })
                    {
                        // This is a new-type; have we seen it before?
                        if let Some(seen_span) = seen_names.get(custom_name.as_str()) {
                            // We have!  Bad news.
                            Err(handler.emit_err(Error::Compile {
                                error: CompileError::RecursiveNewType {
                                    name: custom_name.to_string(),
                                    decl_span: (*seen_span).clone(),
                                    use_span: custom_ty_span.clone(),
                                },
                            }))
                        } else {
                            // We need to add then remove the new path to the 'seen' list; if we
                            // don't remove it we'll get false positives.
                            seen_names.insert(custom_name.as_str(), new_ty_span);
                            let res = inspect_type_names(handler, contract, seen_names, new_ty);
                            seen_names.remove(custom_name.as_str());
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

    /// Lower every `Type::Custom` type in a contract to a proper type.
    fn lower_custom_types_in_contract(&mut self) {
        use std::borrow::BorrowMut;

        // Given a mutable reference to a `Type` and a list of new type declarations `new_types`,
        // replace it or its subtypes with a `Type::Alias` when a `Type::Custom` is encountered
        // that matches the name of a declaration in `new_types`.
        fn replace_custom_type(
            new_types: &fxhash::FxHashMap<String, Type>,
            union_keys: &fxhash::FxHashMap<String, UnionKey>,
            ty: &mut Type,
        ) {
            match ty {
                Type::Custom { name, span } => {
                    if let Some(new_ty) = new_types.get(name) {
                        *ty = Type::Alias {
                            name: name.clone(),
                            ty: Box::new(new_ty.clone()),
                            span: span.clone(),
                        };
                    } else if let Some(decl_key) = union_keys.get(name) {
                        *ty = Type::Union {
                            decl: *decl_key,
                            span: span.clone(),
                        }
                    }
                }

                // Recurse for these types.
                Type::Array { ty, .. } => {
                    replace_custom_type(new_types, union_keys, ty.borrow_mut())
                }

                Type::Tuple { fields, .. } => fields
                    .iter_mut()
                    .for_each(|(_, ty)| replace_custom_type(new_types, union_keys, ty)),

                Type::Alias { ty, .. } => replace_custom_type(new_types, union_keys, ty),

                Type::Map { ty_from, ty_to, .. } => {
                    replace_custom_type(new_types, union_keys, ty_from);
                    replace_custom_type(new_types, union_keys, ty_to);
                }

                Type::Vector { ty, .. } => replace_custom_type(new_types, union_keys, ty),

                Type::Error(_)
                | Type::Unknown(_)
                | Type::Any(_)
                | Type::Primitive { .. }
                | Type::Union { .. } => {}
            }
        }

        let new_types = fxhash::FxHashMap::from_iter(
            self.new_types
                .iter()
                .map(|NewTypeDecl { name, ty, .. }| (name.name.clone(), ty.clone())),
        );

        let union_keys = fxhash::FxHashMap::from_iter(
            self.unions
                .iter()
                .map(|(key, UnionDecl { name, .. })| (name.name.clone(), key)),
        );

        // Replace every single custom type in the contract.
        self.update_types(|ty| replace_custom_type(&new_types, &union_keys, ty), false);
    }
}
