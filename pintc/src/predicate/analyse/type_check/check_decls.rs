use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{Expr, ExternalIntrinsic, Immediate, IntrinsicKind},
    predicate::{
        BlockStatement, ConstraintDecl, Contract, Ident, IfDecl, MatchDecl, MatchDeclBranch,
        PredKey,
    },
    span::Span,
    types::Type,
    warning::Warning,
};
use fxhash::FxHashSet;
use petgraph::{graph::NodeIndex, Graph};
use std::collections::HashMap;

impl Contract {
    pub(in crate::predicate::analyse) fn check_cyclical_predicate_dependencies(
        &self,
        handler: &Handler,
    ) -> Result<(), ErrorEmitted> {
        // This is a dependency graph between predicates. Predicates may depend on other predicates
        // using local predicate calls or `__address_of` intrinsic calls that reference other
        // predicates in the same contract
        let mut dep_graph = Graph::<String, ()>::new();

        // This map keeps track of what node indices (in the dependency graph) are assigned to what
        // predicates (identified by names)
        let mut names_to_indices = HashMap::<String, NodeIndex>::new();

        // This is a map between node indices in the dependency graph and the spans of the
        // corresponding predicate declarations.
        let mut pred_spans = HashMap::<NodeIndex, Span>::new();

        for (_, pred) in self.preds.iter() {
            let new_node = dep_graph.add_node(pred.name.clone());
            names_to_indices.insert(pred.name.clone(), new_node);
            pred_spans.insert(new_node, self.symbols.symbols[&pred.name].clone());
        }

        for (pred_key, pred) in self.preds.iter() {
            // If this predicate refers to another predicate using a `LocalPredicateCall` or an
            // `AddressOf` intrinsic, then create a dependency edge from the other pedicate to this
            // one.
            for expr in self.exprs(pred_key) {
                if let Some(Expr::LocalPredicateCall { predicate, .. }) = expr.try_get(self) {
                    let from = names_to_indices[predicate];
                    let to = names_to_indices[&pred.name];
                    dep_graph.add_edge(from, to, ());
                } else if let Some(Expr::IntrinsicCall {
                    kind: (IntrinsicKind::External(ExternalIntrinsic::AddressOf), _),
                    args,
                    ..
                }) = expr.try_get(self)
                {
                    if let Some(Expr::Immediate {
                        value: Immediate::String(name),
                        ..
                    }) = args.first().and_then(|name| name.try_get(self))
                    {
                        let from = names_to_indices[name];
                        let to = names_to_indices[&pred.name];
                        dep_graph.add_edge(from, to, ());
                    }
                }
            }
        }

        // Now, check if we have any dependency cycles. If so, print those cycles as errors

        // Produce all the strongly connected components of the dependency graph
        let sccs = petgraph::algo::kosaraju_scc(&dep_graph);
        for scc in &sccs {
            // Bad components are ones with more than 1 node. These contain cycles.
            if scc.len() > 1 {
                handler.emit_err(Error::Compile {
                    error: CompileError::DependencyCycle {
                        spans: scc.iter().map(|idx| pred_spans[idx].clone()).collect(),
                    },
                });
            }
        }

        handler.result(())
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
