use crate::{
    error::{CompileError, Error, Handler, LargeTypeError},
    predicate::{
        BlockStatement, ConstraintDecl, Contract, ExprKey, Ident, IfDecl, InterfaceInstance,
        MatchDecl, MatchDeclBranch, PredKey, PredicateInstance,
    },
    types::{b256, Type},
    warning::Warning,
};
use fxhash::FxHashSet;

impl Contract {
    pub(in crate::predicate::analyse) fn check_iface_inst_addrs(&mut self, handler: &Handler) {
        for pred_key in self.preds.keys().collect::<Vec<_>>() {
            // Type check all interface instance declarations.
            let mut addr_keys = Vec::default();
            for InterfaceInstance {
                interface,
                address,
                span,
                ..
            } in &self.preds[pred_key].interface_instances
            {
                if self
                    .interfaces
                    .iter()
                    .any(|e| e.name.to_string() == *interface)
                {
                    // OK. Type check this address below.
                    addr_keys.push(*address);
                } else {
                    handler.emit_err(Error::Compile {
                        error: CompileError::MissingInterface {
                            name: interface.clone(),
                            span: span.clone(),
                        },
                    });
                }
            }

            self.check_instance_addresses(handler, Some(pred_key), &addr_keys);
        }
    }

    pub(in crate::predicate::analyse) fn check_pred_inst_addrs(&mut self, handler: &Handler) {
        for pred_key in self.preds.keys().collect::<Vec<_>>() {
            // Type check all predicate instance declarations.
            let mut addr_keys = Vec::default();
            for PredicateInstance {
                interface_instance,
                predicate,
                address,
                span,
                ..
            } in &self.preds[pred_key].predicate_instances
            {
                if let Some(interface_instance) = interface_instance {
                    // Make sure that an appropriate interface instance exists and an appropriate
                    // predicate interface exists.
                    if let Some(interface_instance) = self.preds[pred_key]
                        .interface_instances
                        .iter()
                        .find(|e| e.name.to_string() == *interface_instance)
                    {
                        if let Some(interface) = self
                            .interfaces
                            .iter()
                            .find(|e| e.name.to_string() == *interface_instance.interface)
                        {
                            if interface
                                .predicate_interfaces
                                .iter()
                                .any(|e| e.name.to_string() == *predicate.to_string())
                            {
                                // OK. Type check this address below.
                                if let Some(address) = *address {
                                    addr_keys.push(address);
                                }
                            } else {
                                handler.emit_err(Error::Compile {
                                    error: CompileError::MissingPredicate {
                                        pred_name: predicate.name.to_string(),
                                        interface_name: Some(interface.name.to_string()),
                                        span: span.clone(),
                                    },
                                });
                            }
                        }
                    } else {
                        handler.emit_err(Error::Compile {
                            error: CompileError::MissingInterfaceInstance {
                                name: interface_instance.clone(),
                                span: span.clone(),
                            },
                        });
                    }
                } else {
                    // This predicate instance must reference a local predicate since
                    // `interface_instance` is `None`. If not, emit an error.

                    // Self referential predicates are not allowed.
                    if "::".to_owned() + &predicate.name == self.preds[pred_key].name {
                        handler.emit_err(Error::Compile {
                            error: CompileError::SelfReferencialPredicate {
                                pred_name: predicate.name.to_string(),
                                span: span.clone(),
                            },
                        });
                    }

                    // If the predicate does not exist, emit an error
                    if self
                        .preds
                        .iter()
                        .all(|(_, pred)| pred.name != "::".to_owned() + &predicate.name)
                    {
                        handler.emit_err(Error::Compile {
                            error: CompileError::MissingPredicate {
                                pred_name: predicate.name.to_string(),
                                interface_name: None,
                                span: span.clone(),
                            },
                        });
                    }
                }
            }

            self.check_instance_addresses(handler, Some(pred_key), &addr_keys);
        }
    }

    fn check_instance_addresses(
        &mut self,
        handler: &Handler,
        pred_key: Option<PredKey>,
        addr_keys: &[ExprKey],
    ) {
        for address in addr_keys {
            if self
                .type_check_single_expr(handler, pred_key, *address)
                .is_ok()
            {
                let ty = address.get_ty(self);
                if !ty.is_b256() {
                    handler.emit_err(Error::Compile {
                        error: CompileError::AddressExpressionTypeError {
                            large_err: Box::new(LargeTypeError::AddressExpressionTypeError {
                                expected_ty: self.with_ctrct(b256()).to_string(),
                                found_ty: self.with_ctrct(ty).to_string(),
                                span: self.expr_key_to_span(*address),
                                expected_span: Some(self.expr_key_to_span(*address)),
                            }),
                        },
                    });
                }
            }
        }
    }

    // Type check an if statement and all of its sub-statements including other ifs. This is a
    // recursive function.
    pub(super) fn type_check_if_decl(
        &mut self,
        handler: &Handler,
        pred_key: Option<PredKey>,
        if_decl: &IfDecl,
    ) {
        let IfDecl {
            condition,
            then_block,
            else_block,
            ..
        } = if_decl;

        // Make sure the condition is a `bool`
        if self
            .type_check_single_expr(handler, pred_key, *condition)
            .is_ok()
        {
            let cond_ty = condition.get_ty(self);
            if !cond_ty.is_bool() {
                handler.emit_err(Error::Compile {
                    error: CompileError::NonBoolConditional {
                        ty: self.with_ctrct(cond_ty).to_string(),
                        conditional: "`if` statement".to_string(),
                        span: self.expr_key_to_span(*condition),
                    },
                });
            }
        }

        // Type check each block statement in the "then" block
        then_block.iter().for_each(|block_statement| {
            self.type_check_block_statement(handler, pred_key, block_statement);
        });

        // Type check each block statement in the "else" block, if available
        else_block.iter().flatten().for_each(|block_statement| {
            self.type_check_block_statement(handler, pred_key, block_statement);
        });
    }

    pub(super) fn type_check_match_decl(
        &mut self,
        handler: &Handler,
        pred_key: Option<PredKey>,
        match_decl: &MatchDecl,
    ) {
        let MatchDecl {
            match_expr,
            match_branches,
            else_branch,
            span,
        } = match_decl;

        if self
            .type_check_single_expr(handler, pred_key, *match_expr)
            .is_ok()
        {
            let union_ty = match_expr.get_ty(self).clone();

            if !union_ty.is_union() {
                handler.emit_err(Error::Compile {
                    error: CompileError::MatchExprNotUnion {
                        found_ty: self.with_ctrct(union_ty).to_string(),
                        span: self.expr_key_to_span(*match_expr),
                    },
                });
            } else {
                let mut variants_set = FxHashSet::default();

                for MatchDeclBranch {
                    name,
                    name_span,
                    binding,
                    block,
                } in match_branches
                {
                    if let Ok(binding_ty) =
                        self.type_check_match_binding(handler, &union_ty, name, name_span, binding)
                    {
                        // Set all sub-exprs to the bound type.
                        if let (Some(binding_id), Some(binding_ty)) = (binding, binding_ty) {
                            for stmt in block {
                                self.set_path_exprs_in_block_to_type(binding_id, &binding_ty, stmt);
                            }
                        }

                        // Then recurse for the block.
                        for stmt in block {
                            self.type_check_block_statement(handler, pred_key, stmt)
                        }
                    }

                    if !variants_set.insert(name[2..].to_string()) {
                        // Variant is already used.
                        handler.emit_err(Error::Compile {
                            error: CompileError::MatchBranchReused {
                                name: name.clone(),
                                span: name_span.clone(),
                            },
                        });
                    }
                }

                if let Some(else_branch) = else_branch {
                    for else_block in else_branch {
                        self.type_check_block_statement(handler, pred_key, else_block);
                    }
                }

                let variant_count = variants_set.len();
                if let Some(union_variant_count) = union_ty.get_union_variant_count(self) {
                    if variant_count < union_variant_count && else_branch.is_none() {
                        // We don't have all variants covered.
                        let mut missing_variants = union_ty.get_union_variant_names(self);
                        missing_variants.retain(|var_name| !variants_set.contains(var_name));
                        handler.emit_err(Error::Compile {
                            error: CompileError::MatchBranchMissing {
                                union_name: self.with_ctrct(union_ty).to_string(),
                                missing_variants,
                                span: span.clone(),
                            },
                        });
                    }
                    if variant_count == union_variant_count
                        && else_branch.is_some()
                        && !handler.has_errors()
                    {
                        // We have all variants accounted for and a superfluous else.
                        // NOTE: It can get confused so we only emit a warning when there are no
                        // other errors.
                        handler.emit_warn(Warning::MatchUnneededElse { span: span.clone() });
                    }
                }
            }
        }
    }

    fn type_check_block_statement(
        &mut self,
        handler: &Handler,
        pred_key: Option<PredKey>,
        block_statement: &BlockStatement,
    ) {
        match block_statement {
            BlockStatement::Constraint(ConstraintDecl { expr, .. }) => {
                let _ = self.type_check_single_expr(handler, pred_key, *expr);
            }

            BlockStatement::If(if_decl) => self.type_check_if_decl(handler, pred_key, if_decl),

            BlockStatement::Match(match_decl) => {
                self.type_check_match_decl(handler, pred_key, match_decl)
            }
        }
    }

    fn set_path_exprs_in_block_to_type(&mut self, name: &Ident, ty: &Type, block: &BlockStatement) {
        match block {
            BlockStatement::Constraint(ConstraintDecl { expr, .. }) => {
                self.set_path_exprs_to_type(name, ty, *expr)
            }

            BlockStatement::If(IfDecl {
                condition,
                then_block,
                else_block,
                ..
            }) => {
                self.set_path_exprs_to_type(name, ty, *condition);

                for stmt in then_block {
                    self.set_path_exprs_in_block_to_type(name, ty, stmt);
                }

                if let Some(else_block) = else_block {
                    for stmt in else_block {
                        self.set_path_exprs_in_block_to_type(name, ty, stmt);
                    }
                }
            }

            BlockStatement::Match(MatchDecl {
                match_expr,
                match_branches,
                else_branch,
                ..
            }) => {
                self.set_path_exprs_to_type(name, ty, *match_expr);

                for MatchDeclBranch {
                    binding: _, block, ..
                } in match_branches
                {
                    // TODO: don't set type if binding shadows this one?  Do we allow it?
                    for stmt in block {
                        self.set_path_exprs_in_block_to_type(name, ty, stmt);
                    }
                }

                if let Some(else_block) = else_branch {
                    for stmt in else_block {
                        self.set_path_exprs_in_block_to_type(name, ty, stmt);
                    }
                }
            }
        }
    }
}
