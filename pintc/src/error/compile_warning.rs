use crate::{
    error::{ErrorLabel, ReportableError},
    span::{empty_span, Span, Spanned},
};
use std::path::PathBuf;
use thiserror::Error;
use yansi::Color;

#[derive(Error, Debug)]
pub enum CompileWarning {
    #[error("compiler internal error: {msg}")]
    Internal { msg: &'static str, span: Span },
    #[error("couldn't read {file}: {error}")]
    FileIO {
        error: std::io::Error,
        file: PathBuf,
        span: Span,
    },
    #[error("multiple source files found for module")]
    DualModulity {
        mod_path: String,
        file_path_a: PathBuf,
        file_path_b: PathBuf,
        span: Span,
    },
    #[error("state is unused")]
    DeadState { span: Span }, // TODO: This is tough and can be confusing depending on if mut is kept or not
    // TODO: dead constraint after constraint is false
    #[error("constraint is always false")]
    FalseConstraint { span: Span },
}

impl ReportableError for CompileWarning {
    fn labels(&self) -> Vec<ErrorLabel> {
        use CompileWarning::*;
        match self {
            DualModulity { mod_path, span, .. } => {
                vec![ErrorLabel {
                    message: format!("multiple source files found for module {mod_path}"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            Internal { msg, span } => {
                if span == &empty_span() {
                    Vec::new()
                } else {
                    vec![ErrorLabel {
                        message: msg.to_string(),
                        span: span.clone(),
                        color: Color::Yellow,
                    }]
                }
            }

            FileIO { .. } => Vec::new(),
            DeadState { span } => {
                vec![ErrorLabel {
                    message: format!("state is unused"),
                    span: span.clone(),
                    color: Color::Yellow,
                }]
            }
            FalseConstraint { span } => {
                vec![ErrorLabel {
                    message: format!("constraint is always false"),
                    span: span.clone(),
                    color: Color::Yellow,
                }]
            }
        }
    }

    fn note(&self) -> Option<String> {
        use CompileWarning::*;
        match self {
            DualModulity {
                file_path_a,
                file_path_b,
                ..
            } => Some(format!(
                "both the files `{}` and `{}` exist, where only one or the other is allowed",
                file_path_a.display(),
                file_path_b.display()
            )),

            Internal { .. } | FileIO { .. } | DeadState { .. } | FalseConstraint { .. } => None,
        }
    }

    fn code(&self) -> Option<String> {
        None
    }

    fn help(&self) -> Option<String> {
        use CompileWarning::*;
        match self {
            // BadCastTo { .. } => Some("casts may only be made to an int or a real".to_string()),
            FalseConstraint { .. } => Some("false constraints are useless".to_string()),
            _ => None,
        }
    }
}

impl Spanned for CompileWarning {
    fn span(&self) -> &Span {
        use CompileWarning::*;
        match self {
            FileIO { span, .. }
            | Internal { span, .. }
            | DualModulity { span, .. }
            | DeadState { span }
            | FalseConstraint { span } => span,
        }
    }
}
