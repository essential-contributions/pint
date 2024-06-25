use super::{Program, ProgramKind};

use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    predicate::{ConstraintDecl, IfDecl, InterfaceInstance, PredicateInstance},
    span::{empty_span, Span},
};

impl Program {
    /// Makes sure that the program structure is correct:
    /// * Stateless programs must have a single Pred that is root Pred and has the name
    /// `Program::ROOT_PRED_NAME`.
    /// * Stateful programs must have at least a two Preds, one of them being the root Pred. The root
    /// Pred cannot have variables, states, constraints, nor solve directives.
    pub fn check_program_kind(self, handler: &Handler) -> Result<Self, ErrorEmitted> {
        fn emit_invalid_decl_err(handler: &Handler, kind: &str, span: &Span) {
            handler.emit_err(Error::Compile {
                error: CompileError::InvalidDeclOutsidePredicateDecl {
                    kind: kind.to_string(),
                    span: span.clone(),
                },
            });
        }

        fn emit_internal_err(handler: &Handler, msg: &'static str) {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg,
                    span: empty_span(),
                },
            });
        }

        match self.kind {
            ProgramKind::Stateless => {
                if self.preds.len() != 1 {
                    emit_internal_err(
                        handler,
                        "stateless contracts cannot have multiple predicate decls",
                    );
                } else if self.preds.iter().next().unwrap().0 != Program::ROOT_PRED_NAME {
                    emit_internal_err(handler, "stateless contracts must have an empty name");
                }
            }
            ProgramKind::Stateful => {
                if self.preds.len() <= 1 {
                    emit_internal_err(
                        handler,
                        "stateful contracts must have at least one predicate decl in addition to the \
                        root predicate",
                    );
                } else {
                    let root_pred = self.root_pred();
                    for (_, var) in root_pred.vars() {
                        emit_invalid_decl_err(handler, "variable", &var.span);
                    }
                    for (_, state) in root_pred.states() {
                        emit_invalid_decl_err(handler, "state", &state.span);
                    }
                    for ConstraintDecl { span, .. } in &root_pred.constraints {
                        emit_invalid_decl_err(handler, "constraint", span);
                    }
                    for IfDecl { span, .. } in &root_pred.if_decls {
                        emit_invalid_decl_err(handler, "`if` statement", span);
                    }
                    for (_, span) in &root_pred.directives {
                        emit_invalid_decl_err(handler, "solve directive", span);
                    }
                    for InterfaceInstance { span, .. } in &root_pred.interface_instances {
                        emit_invalid_decl_err(handler, "interface instance", span);
                    }
                    for PredicateInstance { span, .. } in &root_pred.predicate_instances {
                        handler.emit_err(Error::Compile {
                            error: CompileError::InvalidDeclOutsidePredicateDecl {
                                kind: "predicate instance".to_string(),
                                span: span.clone(),
                            },
                        });
                    }
                }
            }
        };

        if handler.has_errors() {
            return Err(handler.cancel());
        }

        Ok(self)
    }
}
