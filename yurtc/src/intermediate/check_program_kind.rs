use super::{Program, ProgramKind};

use crate::{
    error::{CompileError, Error, Errors},
    span::empty_span,
};

impl Program {
    /// Makes sure that the program structure is correct:
    /// * Stateless programs must have a single II that is root II and has the name
    /// `Program::ROOT_II_NAME`.
    /// * Stateful programs must have at least a two IIs, one of them being the root II. The root
    /// II cannot have variables, states, constraints, nor solve directives.
    pub fn check_program_kind(self) -> Result<Self, Errors> {
        let errors = match self.kind {
            ProgramKind::Stateless => {
                let mut errors = Vec::new();
                if self.iis.len() != 1 {
                    errors.push(Error::Compile {
                        error: CompileError::Internal {
                            msg: "stateless intents cannot have multiple intent decls",
                            span: empty_span(),
                        },
                    });
                } else if self.iis.iter().next().unwrap().0 != Program::ROOT_II_NAME {
                    errors.push(Error::Compile {
                        error: CompileError::Internal {
                            msg: "stateless intent must have an empty name",
                            span: empty_span(),
                        },
                    });
                }
                errors
            }
            ProgramKind::Stateful => {
                let mut errors = Vec::new();
                if self.iis.len() <= 1 {
                    errors.push(Error::Compile {
                        error: CompileError::Internal {
                            msg: "stateful intents must have at least one intent decl \
                              in addition to the root intent",
                            span: empty_span(),
                        },
                    });
                } else {
                    let root_ii = self.root_ii();
                    errors.extend(root_ii.vars.iter().map(|(_, var)| Error::Compile {
                        error: CompileError::InvalidDeclOutsideIntentDecl {
                            kind: "variable".to_string(),
                            span: var.span.clone(),
                        },
                    }));
                    errors.extend(root_ii.states.iter().map(|(_, state)| Error::Compile {
                        error: CompileError::InvalidDeclOutsideIntentDecl {
                            kind: "state".to_string(),
                            span: state.span.clone(),
                        },
                    }));
                    errors.extend(root_ii.constraints.iter().map(|(_, span)| Error::Compile {
                        error: CompileError::InvalidDeclOutsideIntentDecl {
                            kind: "constraint".to_string(),
                            span: span.clone(),
                        },
                    }));
                    errors.extend(root_ii.directives.iter().map(|(_, span)| Error::Compile {
                        error: CompileError::InvalidDeclOutsideIntentDecl {
                            kind: "solve directive".to_string(),
                            span: span.clone(),
                        },
                    }));
                }
                errors
            }
        };

        errors
            .is_empty()
            .then(|| Ok(self))
            .unwrap_or_else(|| Err(Errors(errors)))
    }
}
