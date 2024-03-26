use super::{Program, ProgramKind};

use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    span::empty_span,
};

impl Program {
    /// Makes sure that the program structure is correct:
    /// * Stateless programs must have a single II that is root II and has the name
    /// `Program::ROOT_II_NAME`.
    /// * Stateful programs must have at least a two IIs, one of them being the root II. The root
    /// II cannot have variables, states, constraints, nor solve directives.
    pub fn check_program_kind(self, handler: &Handler) -> Result<Self, ErrorEmitted> {
        match self.kind {
            ProgramKind::Stateless => {
                if self.iis.len() != 1 {
                    handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "stateless intents cannot have multiple intent decls",
                            span: empty_span(),
                        },
                    });
                } else if self.iis.iter().next().unwrap().0 != Program::ROOT_II_NAME {
                    handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "stateless intent must have an empty name",
                            span: empty_span(),
                        },
                    });
                }
            }
            ProgramKind::Stateful => {
                if self.iis.len() <= 1 {
                    handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "stateful intents must have at least one intent decl \
                              in addition to the root intent",
                            span: empty_span(),
                        },
                    });
                } else {
                    let root_ii = self.root_ii();
                    for var in root_ii.vars.values() {
                        handler.emit_err(Error::Compile {
                            error: CompileError::InvalidDeclOutsideIntentDecl {
                                kind: "variable".to_string(),
                                span: var.span.clone(),
                            },
                        });
                    }
                    for state in root_ii.states.values() {
                        handler.emit_err(Error::Compile {
                            error: CompileError::InvalidDeclOutsideIntentDecl {
                                kind: "state".to_string(),
                                span: state.span.clone(),
                            },
                        });
                    }
                    for (_, span) in &root_ii.constraints {
                        handler.emit_err(Error::Compile {
                            error: CompileError::InvalidDeclOutsideIntentDecl {
                                kind: "constraint".to_string(),
                                span: span.clone(),
                            },
                        });
                    }
                    for (_, span) in &root_ii.directives {
                        handler.emit_err(Error::Compile {
                            error: CompileError::InvalidDeclOutsideIntentDecl {
                                kind: "solve directive".to_string(),
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
