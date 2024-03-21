use crate::{
    error::{ErrorLabel, ReportableError},
    span::{Span, Spanned},
};
use std::path::PathBuf;
use thiserror::Error;
use yansi::Color;

#[derive(Error, Debug)]
pub enum CompileError {
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
    #[error("no file found for path")]
    NoFileFoundForPath {
        path_full: String,
        path_mod: String,
        path_enum: String,
        span: Span,
    },
    #[error("macro {name} is declared multiple times")]
    MacroDeclClash {
        name: String,
        span: Span,
        prev_span: Span,
    },
    #[error("macro not found")]
    MacroNotFound { name: String, span: Span },
    #[error("unable to match macro call")]
    MacroCallMismatch {
        name: String,
        arg_count: usize,
        param_counts_descr: String,
        span: Span,
    },
    #[error("macro declared with multiple parameter pack versions")]
    MacroMultiplePacks { span0: Span, span1: Span },
    #[error("unknown parameter pack")]
    MacroUnknownPack {
        actual_pack: Option<(String, Span)>,
        bad_pack: (String, Span),
    },
    #[error("macro `{name}` must have unique parameter counts")]
    MacroNonUniqueParamCounts {
        name: String,
        count: usize,
        span0: Span,
        span1: Span,
    },
    #[error("undefined macro parameter")]
    MacroUndefinedParam { name: String, span: Span },
    #[error("macro call is recursive")]
    MacroRecursion {
        name: String,
        call_span: Span,
        decl_span: Span,
    },
    #[error("`forall` index `{name}` has already been declared")]
    DuplicateForAllIndex {
        name: String,
        span: Span,
        prev_span: Span,
    },
    #[error("invalid bound for `forall` index `{name}`")]
    InvalidForAllIndexBound { name: String, span: Span },
    #[error("cannot find value `{name}` in this scope")]
    SymbolNotFound {
        name: String,
        span: Span,
        enum_names: Vec<String>,
    },
    #[error("attempt to use a non-constant value as an array length")]
    NonConstArrayLength { span: Span },
    #[error("attempt to use an invalid constant as an array length")]
    InvalidConstArrayLength { span: Span },
    #[error("attempt to use a non-constant value as an array index")]
    NonConstArrayIndex { span: Span },
    #[error("attempt to use an invalid constant as an array index")]
    InvalidConstArrayIndex { span: Span },
    #[error("attempt to access array with out of bounds index")]
    ArrayIndexOutOfBounds { span: Span },
    #[error("cannot index into value")]
    CannotIndexIntoValue { span: Span, index_span: Span },
    #[error("unable to determine expression type")]
    UnknownType { span: Span },
    #[error("condition in if-expression must be a boolean")]
    IfCondTypeNotBool(Span),
    #[error("branches in if-expression must have the same type")]
    IfBranchesTypeMismatch { large_err: Box<LargeTypeError> },
    #[error("attempt to access array element from a non-array value")]
    ArrayAccessNonArray { non_array_type: String, span: Span },
    #[error("attempt to index an array with a mismatched value")]
    ArrayAccessWithWrongType {
        found_ty: String,
        expected_ty: String,
        span: Span,
    },
    #[error("comparison between differently sized arrays")]
    MismatchedArrayComparisonSizes {
        op: String,
        lhs_size: i64,
        rhs_size: i64,
        span: Span,
    },
    #[error("attempt to access tuple field from a non-tuple value")]
    TupleAccessNonTuple { non_tuple_type: String, span: Span },
    #[error("invalid tuple accessor")]
    InvalidTupleAccessor {
        accessor: String,
        tuple_type: String,
        span: Span,
    },
    #[error("illegal empty array value")]
    EmptyArrayExpression { span: Span },
    #[error("array element type mismatch")]
    NonHomogeneousArrayElement {
        expected_ty: String,
        ty: String,
        span: Span,
    },
    #[error("{arity} operator type error")]
    OperatorTypeError {
        arity: &'static str,
        large_err: Box<LargeTypeError>,
    },
    #[error("expression has a recursive dependency")]
    ExprRecursion {
        dependant_span: Span,
        dependency_span: Span,
    },
    #[error("invalid cast")]
    BadCastTo { ty: String, span: Span },
    #[error("invalid cast")]
    BadCastFrom { ty: String, span: Span },
    #[error("`solve` directive missing from this project")]
    MissingSolveDirective { span: Span },
}

// This is here purely at the suggestion of Clippy, who pointed out that these error variants are
// quite large and any time you use a `Result` which wraps a `CompileError`, even if there is no
// error, the compiler needs to stack allocate enough space for the largest variant.  These were
// getting above 144 bytes, so instead they're boxed and put in this separate enum.  It's not
// pretty but it'll do for now.

#[derive(Debug)]
pub enum LargeTypeError {
    IfBranchesTypeMismatch {
        then_type: String,
        then_span: Span,
        else_type: String,
        else_span: Span,
        span: Span,
    },
    OperatorTypeError {
        op: &'static str,
        expected_ty: String,
        found_ty: String,
        span: Span,
        expected_span: Option<Span>,
    },
}

impl ReportableError for CompileError {
    fn labels(&self) -> Vec<ErrorLabel> {
        use CompileError::*;
        match self {
            DualModulity { mod_path, span, .. } => {
                vec![ErrorLabel {
                    message: format!("multiple source files found for module {mod_path}"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            NoFileFoundForPath {
                path_full, span, ..
            } => {
                vec![ErrorLabel {
                    message: format!("failed to resolve path {path_full}"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            MacroDeclClash {
                name,
                span,
                prev_span,
            } => {
                vec![
                    ErrorLabel {
                        message: format!("previous declaration of the macro `{name}` here"),
                        span: prev_span.clone(),
                        color: Color::Blue,
                    },
                    ErrorLabel {
                        message: format!(
                            "`{name}` redeclared here with the same number of parameters"
                        ),
                        span: span.clone(),
                        color: Color::Red,
                    },
                ]
            }

            MacroNotFound { name, span } => {
                vec![ErrorLabel {
                    message: format!("macro `{name}` not found"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            MacroCallMismatch { name, span, .. } => {
                vec![ErrorLabel {
                    message: format!("unable to match call to macro `{name}`"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            MacroMultiplePacks { span0, span1 } => {
                vec![
                    ErrorLabel {
                        message: "macro declared here".to_string(),
                        span: span0.clone(),
                        color: Color::Red,
                    },
                    ErrorLabel {
                        message: "and also macro declared here".to_string(),
                        span: span1.clone(),
                        color: Color::Red,
                    },
                ]
            }

            MacroUnknownPack {
                actual_pack,
                bad_pack,
            } => {
                let mut labels = vec![ErrorLabel {
                    message: format!("unknown parameter pack `{}`", bad_pack.0),
                    span: bad_pack.1.clone(),
                    color: Color::Red,
                }];

                if let Some((name, span)) = actual_pack {
                    labels.push(ErrorLabel {
                        message: format!("actual parameter pack is `{name}`"),
                        span: span.clone(),
                        color: Color::Blue,
                    });
                }

                labels
            }

            MacroNonUniqueParamCounts {
                count,
                span0,
                span1,
                ..
            } => {
                vec![
                    ErrorLabel {
                        message: format!("macro declared here has {count} parameters"),
                        span: span0.clone(),
                        color: Color::Red,
                    },
                    ErrorLabel {
                        message: format!("macro declared here has {count} parameters"),
                        span: span1.clone(),
                        color: Color::Red,
                    },
                ]
            }

            MacroUndefinedParam { name, span } => {
                vec![ErrorLabel {
                    message: format!("undefined parameter `{name}` in macro body"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            MacroRecursion {
                name,
                call_span,
                decl_span,
            } => {
                vec![
                    ErrorLabel {
                        message: format!("macro `{name}` is recursively called"),
                        span: call_span.clone(),
                        color: Color::Red,
                    },
                    ErrorLabel {
                        message: format!("macro `{name}` declared here"),
                        span: decl_span.clone(),
                        color: Color::Blue,
                    },
                ]
            }

            DuplicateForAllIndex {
                name,
                span,
                prev_span,
            } => {
                vec![
                    ErrorLabel {
                        message: format!("previous declaration of the index `{name}` here"),
                        span: prev_span.clone(),
                        color: Color::Blue,
                    },
                    ErrorLabel {
                        message: format!("`{name}` redeclared here"),
                        span: span.clone(),
                        color: Color::Red,
                    },
                ]
            }

            InvalidForAllIndexBound { name, span } => {
                vec![ErrorLabel {
                    message: format!("invalid bound for `forall` index `{name}`"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            SymbolNotFound { span, .. } => {
                vec![ErrorLabel {
                    message: "not found in this scope".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            NonConstArrayLength { span } | NonConstArrayIndex { span } => {
                vec![ErrorLabel {
                    message: "this must be a constant".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            InvalidConstArrayLength { span } => {
                vec![ErrorLabel {
                    message: "this must be a strictly positive integer value".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            InvalidConstArrayIndex { span } => {
                vec![ErrorLabel {
                    message: "this must be a non-negative integer value".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            ArrayIndexOutOfBounds { span } => {
                vec![ErrorLabel {
                    message: "array index is out of bounds".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            CannotIndexIntoValue { span, index_span } => {
                vec![
                    ErrorLabel {
                        message: "this must be an array".to_string(),
                        span: span.clone(),
                        color: Color::Blue,
                    },
                    ErrorLabel {
                        message: "invalid indexing here".to_string(),
                        span: index_span.clone(),
                        color: Color::Red,
                    },
                ]
            }

            UnknownType { span } => vec![ErrorLabel {
                message: "type of this expression is ambiguous".to_string(),
                span: span.clone(),
                color: Color::Red,
            }],

            IfCondTypeNotBool(span) => {
                vec![ErrorLabel {
                    message: "condition must be a boolean".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            ArrayAccessNonArray {
                non_array_type,
                span,
            } => {
                vec![ErrorLabel {
                    message: format!("value must be an array; found `{non_array_type}`"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            ArrayAccessWithWrongType {
                expected_ty, span, ..
            } => {
                vec![ErrorLabel {
                    message: if expected_ty == "int" {
                        "array access must be with an int value".to_string()
                    } else {
                        format!("array access must be with a `{expected_ty}` variant")
                    },
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            MismatchedArrayComparisonSizes { span, .. } => vec![ErrorLabel {
                message: "cannot compare arrays of different sizes".to_string(),
                span: span.clone(),
                color: Color::Red,
            }],

            TupleAccessNonTuple {
                non_tuple_type,
                span,
            } => {
                vec![ErrorLabel {
                    message: format!("value must be a tuple; found `{non_tuple_type}`"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            InvalidTupleAccessor { accessor, span, .. } => vec![ErrorLabel {
                message: format!("unable to get field from tuple using `{accessor}`"),
                span: span.clone(),
                color: Color::Red,
            }],

            EmptyArrayExpression { span } => vec![ErrorLabel {
                message: "empty array values are illegal".to_string(),
                span: span.clone(),
                color: Color::Red,
            }],

            NonHomogeneousArrayElement { ty, span, .. } => {
                vec![ErrorLabel {
                    message: format!("array element has type `{ty}`"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            ExprRecursion {
                dependant_span,
                dependency_span,
            } => vec![
                ErrorLabel {
                    message: "cannot determine type of expression due to dependency".to_string(),
                    span: dependant_span.clone(),
                    color: Color::Red,
                },
                ErrorLabel {
                    message: "dependency on expression is recursive".to_string(),
                    span: dependency_span.clone(),
                    color: Color::Red,
                },
            ],

            IfBranchesTypeMismatch { large_err } | OperatorTypeError { large_err, .. } => {
                match &**large_err {
                    LargeTypeError::IfBranchesTypeMismatch {
                        then_type,
                        then_span,
                        else_type,
                        else_span,
                        ..
                    } => vec![
                        ErrorLabel {
                            message: format!("'then' branch has the type `{then_type}`"),
                            span: then_span.clone(),
                            color: Color::Red,
                        },
                        ErrorLabel {
                            message: format!("'else' branch has the type `{else_type}`"),
                            span: else_span.clone(),
                            color: Color::Red,
                        },
                    ],

                    LargeTypeError::OperatorTypeError {
                        op,
                        found_ty,
                        expected_ty,
                        span,
                        expected_span,
                        ..
                    } => {
                        let mut labels = vec![ErrorLabel {
                            message: format!(
                                "operator `{op}` argument has unexpected type `{found_ty}`"
                            ),
                            span: span.clone(),
                            color: Color::Red,
                        }];

                        if let Some(span) = expected_span {
                            labels.push(ErrorLabel {
                                message: format!("expecting type `{expected_ty}`"),
                                span: span.clone(),
                                color: Color::Blue,
                            });
                        }

                        labels
                    }
                }
            }

            BadCastTo { ty, span } => vec![ErrorLabel {
                message: format!("illegal cast to a `{ty}`"),
                span: span.clone(),
                color: Color::Red,
            }],

            BadCastFrom { ty, span } => vec![ErrorLabel {
                message: format!("illegal cast from a `{ty}`"),
                span: span.clone(),
                color: Color::Red,
            }],

            MissingSolveDirective { span } => vec![ErrorLabel {
                message: "`solve` directive missing from this file".to_string(),
                span: span.clone(),
                color: Color::Red,
            }],

            Internal { .. } | FileIO { .. } => Vec::new(),
        }
    }

    fn note(&self) -> Option<String> {
        use CompileError::*;
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

            NoFileFoundForPath {
                path_mod,
                path_enum,
                ..
            } => Some(format!(
                "one of the modules `{path_mod}` or `{path_enum}` must exist",
            )),

            MacroDeclClash { name, .. } => Some(format!(
                "it is valid to have multiple macros named `{name}` \
                but they must have differing parameter lists"
            )),

            MacroCallMismatch {
                arg_count,
                param_counts_descr,
                ..
            } => {
                // foobar
                Some(format!(
                    "the valid number of arguments may be {param_counts_descr} \
                        but this call passes {arg_count} arguments"
                ))
            }

            MacroRecursion { .. } => Some(
                "a macro called recursively with the same number of arguments \
                    will cause a non-terminating loop during expansion"
                    .to_string(),
            ),

            DuplicateForAllIndex { name, .. } => Some(format!(
                "`forall` index `{name}` must be declared only once in this scope"
            )),

            InvalidForAllIndexBound { .. } => {
                Some("`forall` index bound must be an integer literal".to_string())
            }

            MismatchedArrayComparisonSizes {
                op,
                lhs_size,
                rhs_size,
                ..
            } => Some(format!(
                "the left-hand side argument of the `{op}` operator has {lhs_size} \
                    elements while the right-hand side argument has {rhs_size} elements"
            )),

            InvalidTupleAccessor { tuple_type, .. } => {
                Some(format!("tuple has type `{tuple_type}`"))
            }

            NonHomogeneousArrayElement { expected_ty, .. } => {
                Some(format!("expecting array element type `{expected_ty}`"))
            }

            ArrayAccessWithWrongType { found_ty, .. } => {
                Some(format!("found access using type `{found_ty}`"))
            }

            MissingSolveDirective { .. } => {
                Some("solve` directive must appear exactly once in a project and must appear in the top level module".to_string())
            }

            // solve` directive must appear exactly once in a project and must appear in the top level module

            Internal { .. }
            | FileIO { .. }
            | MacroNotFound { .. }
            | MacroUndefinedParam { .. }
            | SymbolNotFound { .. }
            | NonConstArrayLength { .. }
            | InvalidConstArrayLength { .. }
            | NonConstArrayIndex { .. }
            | InvalidConstArrayIndex { .. }
            | ArrayIndexOutOfBounds { .. }
            | CannotIndexIntoValue { .. }
            | MacroMultiplePacks { .. }
            | MacroUnknownPack { .. }
            | MacroNonUniqueParamCounts { .. }
            | UnknownType { .. }
            | IfCondTypeNotBool(_)
            | IfBranchesTypeMismatch { .. }
            | OperatorTypeError { .. }
            | ArrayAccessNonArray { .. }
            | TupleAccessNonTuple { .. }
            | EmptyArrayExpression { .. }
            | ExprRecursion { .. }
            | BadCastTo { .. }
            | BadCastFrom { .. } => None,
        }
    }

    fn code(&self) -> Option<String> {
        None
    }

    fn help(&self) -> Option<String> {
        use CompileError::*;
        match self {
            SymbolNotFound { enum_names, .. } if !enum_names.is_empty() => {
                Some(format!(
                    "this symbol is a variant of enum{} {} and may need a fully qualified path",
                    if enum_names.len() > 1 { "s" } else { "" },
                    enum_names
                        .iter()
                        .enumerate()
                        .map(|(idx, enum_name)| {
                            if idx + 2 == enum_names.len() {
                                // 2nd last
                                format!("`{enum_name}` and ")
                            } else if idx + 1 == enum_names.len() {
                                // last
                                format!("`{enum_name}`")
                            } else {
                                // otherwise...
                                format!("`{enum_name}`, ")
                            }
                        })
                        .collect::<Vec<_>>()
                        .join("")
                ))
            }

            MacroCallMismatch { name, .. } => Some(format!(
                "a macro named `{name}` is defined but not with the required \
                signature to fulfill this call"
            )),

            BadCastTo { .. } => Some("casts may only be made to an int or a real".to_string()),
            BadCastFrom { .. } => Some(
                "casts may only be made from an int to a real, from a bool to an int or \
                from an enum to an int"
                    .to_string(),
            ),

            _ => None,
        }
    }
}

impl Spanned for CompileError {
    fn span(&self) -> &Span {
        use CompileError::*;
        match self {
            FileIO { span, .. }
            | Internal { span, .. }
            | DualModulity { span, .. }
            | NoFileFoundForPath { span, .. }
            | MacroDeclClash { span, .. }
            | MacroNotFound { span, .. }
            | MacroCallMismatch { span, .. }
            | MacroMultiplePacks { span0: span, .. }
            | MacroUnknownPack {
                bad_pack: (_, span),
                ..
            }
            | MacroNonUniqueParamCounts { span0: span, .. }
            | MacroUndefinedParam { span, .. }
            | MacroRecursion {
                call_span: span, ..
            }
            | DuplicateForAllIndex { span, .. }
            | InvalidForAllIndexBound { span, .. }
            | SymbolNotFound { span, .. }
            | NonConstArrayIndex { span }
            | InvalidConstArrayLength { span }
            | NonConstArrayLength { span }
            | InvalidConstArrayIndex { span }
            | ArrayIndexOutOfBounds { span }
            | CannotIndexIntoValue { span, .. }
            | UnknownType { span }
            | IfCondTypeNotBool(span)
            | ArrayAccessNonArray { span, .. }
            | ArrayAccessWithWrongType { span, .. }
            | MismatchedArrayComparisonSizes { span, .. }
            | TupleAccessNonTuple { span, .. }
            | InvalidTupleAccessor { span, .. }
            | EmptyArrayExpression { span }
            | NonHomogeneousArrayElement { span, .. }
            | ExprRecursion {
                dependant_span: span,
                ..
            }
            | BadCastTo { span, .. }
            | BadCastFrom { span, .. }
            | MissingSolveDirective { span, .. } => span,

            IfBranchesTypeMismatch { large_err } | OperatorTypeError { large_err, .. } => {
                match &**large_err {
                    LargeTypeError::IfBranchesTypeMismatch { span, .. }
                    | LargeTypeError::OperatorTypeError { span, .. } => span,
                }
            }
        }
    }
}
