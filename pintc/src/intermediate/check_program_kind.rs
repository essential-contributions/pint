use super::{Program, ProgramKind};

use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    intermediate::{ConstraintDecl, IfDecl, IntentInstance, InterfaceInstance},
    span::{empty_span, Span},
};

impl Program {
    /// Makes sure that the program structure is correct:
    /// * Stateless programs must have a single II that is root II and has the name
    /// `Program::ROOT_II_NAME`.
    /// * Stateful programs must have at least a two IIs, one of them being the root II. The root
    /// II cannot have variables, states, constraints, nor solve directives.
    pub fn check_program_kind(self, handler: &Handler) -> Result<Self, ErrorEmitted> {
        fn emit_invalid_decl_err(handler: &Handler, kind: &str, span: &Span) {
            handler.emit_err(Error::Compile {
                error: CompileError::InvalidDeclOutsideIntentDecl {
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
                if self.iis.len() != 1 {
                    emit_internal_err(
                        handler,
                        "stateless intents cannot have multiple intent decls",
                    );
                } else if self.iis.iter().next().unwrap().0 != Program::ROOT_II_NAME {
                    emit_internal_err(handler, "stateless intent must have an empty name");
                }
            }
            ProgramKind::Stateful => {
                if self.iis.len() <= 1 {
                    emit_internal_err(
                        handler,
                        "stateful intents must have at least one intent decl in addition to the \
                        root intent",
                    );
                } else {
                    let root_ii = self.root_ii();
                    for (_, var) in root_ii.vars() {
                        emit_invalid_decl_err(handler, "variable", &var.span);
                    }
                    for (_, state) in root_ii.states() {
                        emit_invalid_decl_err(handler, "state", &state.span);
                    }
                    for ConstraintDecl { span, .. } in &root_ii.constraints {
                        emit_invalid_decl_err(handler, "constraint", span);
                    }
                    for IfDecl { span, .. } in &root_ii.if_decls {
                        emit_invalid_decl_err(handler, "`if` statement", span);
                    }
                    for (_, span) in &root_ii.directives {
                        emit_invalid_decl_err(handler, "solve directive", span);
                    }
                    for InterfaceInstance { span, .. } in &root_ii.interface_instances {
                        emit_invalid_decl_err(handler, "interface instance", span);
                    }
                    for IntentInstance { span, .. } in &root_ii.intent_instances {
                        handler.emit_err(Error::Compile {
                            error: CompileError::InvalidDeclOutsideIntentDecl {
                                kind: "intent instance".to_string(),
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
