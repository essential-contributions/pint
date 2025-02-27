use crate::{
    error::{ErrorLabel, ReportableError},
    span::{Span, Spanned},
};
use std::path::PathBuf;
use thiserror::Error;
use yansi::Color;

#[derive(Error, Debug)]
pub enum CompileError {
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
        path_union: String,
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
        suggestion: Option<String>,
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
    #[error("undefined spliced variable")]
    MacroUnrecognizedSpliceVar { var_name: String, span: Span },
    #[error("spliced variable `{var_name}` must be an array")]
    MacroSpliceVarNotArray { var_name: String, span: Span },
    #[error("unable to determine spliced array size")]
    MacroSpliceArrayUnknownSize { var_name: String, span: Span },
    #[error("macro call is not an expression")]
    MacroCallWasNotExpression { span: Span },
    #[error("`{gen_kind}` index `{name}` has already been declared")]
    DuplicateGeneratorIndex {
        name: String,
        gen_kind: String,
        span: Span,
        prev_span: Span,
    },
    #[error("invalid bound for `{gen_kind}` index `{name}`")]
    InvalidGeneratorIndexBound {
        name: String,
        gen_kind: String,
        span: Span,
    },
    #[error("range for `{gen_kind}` must be an `int`")]
    NonIntGeneratorRange {
        ty: String,
        gen_kind: String,
        span: Span,
    },
    #[error("condition for `{gen_kind}` must be a `bool`")]
    NonBoolGeneratorCondition {
        ty: String,
        gen_kind: String,
        span: Span,
    },
    #[error("body for `{gen_kind}` must be a `bool` expression")]
    NonBoolGeneratorBody {
        ty: String,
        gen_kind: String,
        span: Span,
    },
    #[error("cannot find value `{name}` in this scope")]
    SymbolNotFound {
        name: String,
        span: Span,
        union_names: Vec<String>,
    },
    #[error("cannot find storage variable `{name}`")]
    StorageSymbolNotFound { name: String, span: Span },
    #[error("cannot find storage variable `{name}`")]
    MissingStorageBlock { name: String, span: Span },
    #[error("`next state` access must be bound to a variable or to a storage access")]
    InvalidNextStateAccess { span: Span },
    #[error("cannot find interface declaration `{name}`")]
    MissingInterface { name: String, span: Span },
    #[error("cannot find predicate `{pred_name}` in {}",
        if let Some(interface_name) = interface_name {
            format!("interface `{interface_name}`")
        } else {
            "this contract".to_string()
        }
    )]
    MissingPredicate {
        pred_name: String,
        interface_name: Option<String>,
        span: Span,
    },
    #[error("self referential predicate `{pred_name}`")]
    SelfReferencialPredicate { pred_name: String, span: Span },
    #[error("address expression type error")]
    AddressExpressionTypeError { large_err: Box<LargeTypeError> },
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
    #[error("undefined type")]
    UndefinedType { span: Span },
    #[error("uninferrable type")]
    UninferrableType { span: Span },
    #[error("condition for {conditional} must be a `bool`")]
    NonBoolConditional {
        ty: String,
        conditional: String,
        span: Span,
    },
    #[error("branches of a select expression must have the same type")]
    SelectBranchesTypeMismatch { large_err: Box<LargeTypeError> },
    #[error("constraint expression type error")]
    ConstraintExpressionTypeError { large_err: Box<LargeTypeError> },
    #[error("indexed expression invalid")]
    IndexExprNonIndexable {
        non_indexable_type: String,
        span: Span,
    },
    #[error("attempt to index an array with a mismatched value")]
    ArrayAccessWithWrongType {
        found_ty: String,
        expected_ty: String,
        span: Span,
    },
    #[error("invalid array range type {found_ty}")]
    InvalidArrayRangeType { found_ty: String, span: Span },
    #[error("predicate parameters cannot have storage types")]
    ParamHasStorageType {
        ty: String,
        nested_ty: String,
        span: Span,
    },
    #[error("local variables cannot have storage types")]
    VarHasStorageType {
        ty: String,
        nested_ty: String,
        span: Span,
    },
    #[error("type not allowed in storage")]
    TypeNotAllowedInStorage { ty: String, span: Span },
    #[error("attempt to index a storage map with a mismatched value")]
    StorageMapAccessWithWrongType {
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
    #[error("operator invalid type error")]
    OperatorInvalidType {
        op: &'static str,
        ty_kind: &'static str,
        bad_ty: String,
        span: Span,
    },
    #[error("{init_kind} initialization type error")]
    InitTypeError {
        init_kind: &'static str,
        large_err: Box<LargeTypeError>,
    },
    #[error("variable initialization type error")]
    VarInitTypeError { large_err: Box<LargeTypeError> },
    #[error("expression has a recursive dependency")]
    ExprRecursion {
        dependant_span: Span,
        dependency_span: Span,
    },
    #[error("invalid cast")]
    BadCastTo { ty: String, span: Span },
    #[error("invalid cast")]
    BadCastFrom { ty: String, span: Span },
    #[error("left and right types in range differ")]
    RangeTypesMismatch {
        lb_ty: String,
        ub_ty: String,
        span: Span,
    },
    #[error("range type must be numeric")]
    RangeTypesNonNumeric { ty: String, span: Span },
    #[error("value type and range type differ")]
    InExprTypesMismatch {
        val_ty: String,
        range_ty: String,
        span: Span,
    },
    #[error("value type and array element type in range differ")]
    InExprTypesArrayMismatch {
        val_ty: String,
        el_ty: String,
        span: Span,
    },
    #[error("this intrinsic takes {} but {}",
        if *expected == 1 {
            format!("{expected} argument")
        } else {
            format!("{expected} arguments")
        },
        if *found == 1 {
            format!("{found} argument was supplied")
        } else {
            format!("{found} arguments were supplied")
        }
    )]
    UnexpectedIntrinsicArgCount {
        expected: usize,
        found: usize,
        span: Span,
    },
    #[error("this predicate takes {} but {}",
        if *expected == 1 {
            format!("{expected} argument")
        } else {
            format!("{expected} arguments")
        },
        if *found == 1 {
            format!("{found} argument was supplied")
        } else {
            format!("{found} arguments were supplied")
        }
    )]
    UnexpectedPredicateArgCount {
        expected: usize,
        found: usize,
        span: Span,
    },
    #[error("incorrect intrinsic argument")]
    MismatchedIntrinsicArgType {
        expected: String,
        found: String,
        intrinsic_span: Span,
        arg_span: Span,
    },
    #[error("incorrect predicate argument")]
    MismatchedPredicateArgType {
        expected: String,
        found: String,
        span: Span,
        arg_span: Span,
    },
    #[error("type alias refers to itself")]
    RecursiveNewType {
        name: String,
        decl_span: Span,
        use_span: Span,
    },
    #[error("dependency cycle detected between predicates")]
    DependencyCycle { spans: Vec<Span> },
    #[error("dependency cycle detected between variables")]
    VarsDependencyCycle { spans: Vec<Span> },
    #[error("intrinsic `__address_of` cannot refer to the predicate it's used in")]
    AddressOfSelf { name: String, span: Span },
    #[error("predicate `{name}` not found")]
    PredicateNameNotFound { name: String, span: Span },
    #[error("match expression not a union")]
    MatchExprNotUnion { found_ty: String, span: Span },
    #[error("unknown union variant name")]
    MatchVariantUnknown {
        variant: String,
        union_name: String,
        actual_variants: Vec<String>,
        span: Span,
    },
    #[error("match branch type mismatch")]
    MatchBranchTypeMismatch {
        expected_ty: String,
        found_ty: String,
        span: Span,
    },
    #[error("re-used match variant")]
    MatchBranchReused { name: String, span: Span },
    #[error("not all match variants are covered")]
    MatchBranchMissing {
        union_name: String,
        missing_variants: Vec<String>,
        span: Span,
    },
    #[error("unknown union")]
    UnknownUnion { name: String, span: Span },
    #[error("unknown union variant")]
    UnknownUnionVariant {
        name: String,
        valid_variants: Vec<String>,
        span: Span,
    },
    #[error("union variant does not have a value")]
    SuperfluousUnionExprValue { name: String, span: Span },
    #[error("missing union variant value")]
    MissingUnionExprValue {
        name: String,
        variant_ty: String,
        span: Span,
    },
    #[error("union variant type mismatch")]
    UnionVariantTypeMismatch {
        found_ty: String,
        expected_ty: String,
        span: Span,
    },
    #[error("invalid position for accessing storage")]
    InvalidStorageAccess { span: Span },
    #[error("identical predicates found in the same contract")]
    IdenticalPredicates {
        original_name: String,
        duplicate_name: String,
        original_span: Span,
        span: Span,
    },
    #[error("invalid map range type")]
    InvalidMapRangeType { found_ty: String, span: Span },
    #[error("attempt to use a non-constant value in a constant")]
    InvalidConst { span: Span },
}

// This is here purely at the suggestion of Clippy, who pointed out that these error variants are
// quite large and any time you use a `Result` which wraps a `CompileError`, even if there is no
// error, the compiler needs to stack allocate enough space for the largest variant.  These were
// getting above 144 bytes, so instead they're boxed and put in this separate enum.  It's not
// pretty but it'll do for now.

#[derive(Debug)]
pub enum LargeTypeError {
    SelectBranchesTypeMismatch {
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
    VarInitTypeError {
        expected_ty: String,
        found_ty: String,
        span: Span,
        expected_span: Option<Span>,
    },
    ConstraintExpressionTypeError {
        expected_ty: String,
        found_ty: String,
        span: Span,
        expected_span: Option<Span>,
    },
    AddressExpressionTypeError {
        expected_ty: String,
        found_ty: String,
        span: Span,
        expected_span: Option<Span>,
    },
    InitTypeError {
        init_kind: &'static str,
        expected_ty: String,
        found_ty: String,
        expected_ty_span: Span,
        init_span: Span,
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

            MacroUnrecognizedSpliceVar { var_name, span } => {
                vec![ErrorLabel {
                    message: format!("unable to splice unknown variable `{var_name}`"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            MacroSpliceVarNotArray { var_name, span } => {
                vec![ErrorLabel {
                    message: format!("unable to splice non-array variable `{var_name}`"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            MacroSpliceArrayUnknownSize { var_name, span } => {
                vec![ErrorLabel {
                    message: format!(
                        "unable to determine spliced array size for `{var_name}` while parsing"
                    ),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            MacroCallWasNotExpression { span } => {
                vec![ErrorLabel {
                    message: "macro call does not expand to an expression".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            DuplicateGeneratorIndex {
                name,
                span,
                prev_span,
                ..
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

            InvalidGeneratorIndexBound {
                name,
                gen_kind,
                span,
            } => {
                vec![ErrorLabel {
                    message: format!("invalid bound for `{gen_kind}` index `{name}`"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            NonIntGeneratorRange { ty, span, .. } => vec![ErrorLabel {
                message: format!("invalid type `{ty}`, expecting `int`"),
                span: span.clone(),
                color: Color::Red,
            }],

            NonBoolConditional { ty, span, .. }
            | NonBoolGeneratorCondition { ty, span, .. }
            | NonBoolGeneratorBody { ty, span, .. } => {
                vec![ErrorLabel {
                    message: format!("invalid type `{ty}`, expecting `bool`"),
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

            StorageSymbolNotFound { span, .. } => {
                vec![ErrorLabel {
                    message: "not found in storage declaration".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            MissingStorageBlock { span, .. } => {
                vec![ErrorLabel {
                    message: "no storage declaration found".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            InvalidNextStateAccess { span } => {
                vec![ErrorLabel {
                    message:
                        "`next state` access must be bound to a variable or to a storage access"
                            .to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            MissingInterface { name, span } => {
                vec![ErrorLabel {
                    message: format!("cannot find interface declaration `{name}`"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            MissingPredicate {
                pred_name,
                interface_name,
                span,
            } => {
                vec![ErrorLabel {
                    message: format!(
                        "this predicate call references predicate `{pred_name}` \
                         which does not exist in {}",
                        if let Some(interface_name) = interface_name {
                            format!("interface `{interface_name}`")
                        } else {
                            "this contract".to_string()
                        }
                    ),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            SelfReferencialPredicate { span, .. } => {
                vec![ErrorLabel {
                    message: "this predicate call references the predicate it's declared in"
                        .to_string(),
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

            UndefinedType { span } => vec![ErrorLabel {
                message: "type is undefined".to_string(),
                span: span.clone(),
                color: Color::Red,
            }],

            UninferrableType { span } => vec![ErrorLabel {
                message: "type of this expression cannot be inferred".to_string(),
                span: span.clone(),
                color: Color::Red,
            }],

            IndexExprNonIndexable {
                non_indexable_type,
                span,
            } => {
                vec![ErrorLabel {
                    message: format!(
                        "value must be an array or a storage map; found `{non_indexable_type}`"
                    ),
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

            InvalidArrayRangeType { span, .. } => {
                vec![ErrorLabel {
                    message: "array access must be of type `int` or enumeration `union`"
                        .to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            ParamHasStorageType { ty, span, .. } => {
                vec![ErrorLabel {
                    message: format!("found parameter of storage type {ty} here"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            VarHasStorageType { ty, span, .. } => {
                vec![ErrorLabel {
                    message: format!("found local variable of storage type {ty} here"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            TypeNotAllowedInStorage { ty, span, .. } => {
                vec![ErrorLabel {
                    message: format!("found type {ty} in storage"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            StorageMapAccessWithWrongType {
                expected_ty, span, ..
            } => {
                vec![ErrorLabel {
                    message: format!("storage map access must be with a `{expected_ty}` variant"),
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

            SelectBranchesTypeMismatch { large_err }
            | OperatorTypeError { large_err, .. }
            | VarInitTypeError { large_err, .. }
            | ConstraintExpressionTypeError { large_err, .. }
            | AddressExpressionTypeError { large_err, .. }
            | InitTypeError { large_err, .. } => match large_err.as_ref() {
                LargeTypeError::SelectBranchesTypeMismatch {
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
                    let what = format!("operator `{op}` argument");

                    generate_type_error_labels(&what, found_ty, expected_ty, span, expected_span)
                }

                LargeTypeError::VarInitTypeError {
                    found_ty,
                    expected_ty,
                    span,
                    expected_span,
                    ..
                } => generate_type_error_labels(
                    "initializing expression",
                    found_ty,
                    expected_ty,
                    span,
                    expected_span,
                ),

                LargeTypeError::ConstraintExpressionTypeError {
                    found_ty,
                    expected_ty,
                    span,
                    expected_span,
                    ..
                } => generate_type_error_labels(
                    "constraint expression",
                    found_ty,
                    expected_ty,
                    span,
                    expected_span,
                ),

                LargeTypeError::AddressExpressionTypeError {
                    found_ty,
                    expected_ty,
                    span,
                    expected_span,
                    ..
                } => generate_type_error_labels(
                    "address expression",
                    found_ty,
                    expected_ty,
                    span,
                    expected_span,
                ),

                LargeTypeError::InitTypeError {
                    init_kind,
                    expected_ty,
                    found_ty,
                    expected_ty_span,
                    init_span,
                } => {
                    let what = format!("{init_kind} initializer");

                    generate_type_error_labels(
                        &what,
                        found_ty,
                        expected_ty,
                        init_span,
                        &Some(expected_ty_span.clone()),
                    )
                }
            },

            OperatorInvalidType {
                op,
                ty_kind,
                bad_ty,
                span,
            } => vec![ErrorLabel {
                message: format!("invalid {ty_kind} type `{bad_ty}` for operator `{op}`"),
                span: span.clone(),
                color: Color::Red,
            }],

            BadCastTo { ty, span } => vec![ErrorLabel {
                message: format!("illegal cast to `{ty}`"),
                span: span.clone(),
                color: Color::Red,
            }],

            BadCastFrom { ty, span } => vec![ErrorLabel {
                message: format!("illegal cast from `{ty}`"),
                span: span.clone(),
                color: Color::Red,
            }],

            RangeTypesMismatch { lb_ty, ub_ty, span } => vec![ErrorLabel {
                message: format!("expecting `{lb_ty}` type , found `{ub_ty}` type"),
                span: span.clone(),
                color: Color::Red,
            }],

            RangeTypesNonNumeric { ty, span } => vec![ErrorLabel {
                message: format!("ranges must have numeric bounds; found `{ty}`"),
                span: span.clone(),
                color: Color::Red,
            }],

            InExprTypesMismatch {
                val_ty,
                range_ty,
                span,
            } => vec![ErrorLabel {
                message: format!(
                    "range type mismatch; expecting `{val_ty}` type, found `{range_ty}` type"
                ),
                span: span.clone(),
                color: Color::Red,
            }],

            InExprTypesArrayMismatch {
                val_ty,
                el_ty,
                span,
            } => vec![ErrorLabel {
                message: format!(
                    "array element type mismatch; expecting `{val_ty}` type, found `{el_ty}` type"
                ),
                span: span.clone(),
                color: Color::Red,
            }],

            UnexpectedIntrinsicArgCount { span, .. } => vec![ErrorLabel {
                message: "unexpected number of arguments here".to_string(),
                span: span.clone(),
                color: Color::Red,
            }],

            UnexpectedPredicateArgCount { span, .. } => vec![ErrorLabel {
                message: "unexpected number of arguments here".to_string(),
                span: span.clone(),
                color: Color::Red,
            }],

            MismatchedIntrinsicArgType {
                expected,
                found,
                intrinsic_span,
                arg_span,
            } => vec![
                ErrorLabel {
                    message: format!("expected `{expected}`, found `{found}`"),
                    span: arg_span.clone(),
                    color: Color::Blue,
                },
                ErrorLabel {
                    message: "arguments to this intrinsic are incorrect`".to_string(),
                    span: intrinsic_span.clone(),
                    color: Color::Red,
                },
            ],

            MismatchedPredicateArgType {
                expected,
                found,
                span,
                arg_span,
            } => vec![
                ErrorLabel {
                    message: format!("expected `{expected}`, found `{found}`"),
                    span: arg_span.clone(),
                    color: Color::Blue,
                },
                ErrorLabel {
                    message: "arguments to this predicate call are incorrect`".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                },
            ],

            RecursiveNewType {
                name,
                decl_span,
                use_span,
            } => vec![
                ErrorLabel {
                    message: format!("type alias `{name}` is used recursively in declaration"),
                    span: use_span.clone(),
                    color: Color::Red,
                },
                ErrorLabel {
                    message: format!("`{name}` is declared here"),
                    span: decl_span.clone(),
                    color: Color::Blue,
                },
            ],

            DependencyCycle { spans } => spans
                .iter()
                .map(|span| ErrorLabel {
                    message: "this predicate is on the dependency cycle".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                })
                .collect::<Vec<_>>(),

            VarsDependencyCycle { spans } => spans
                .iter()
                .map(|span| ErrorLabel {
                    message: "this variable is on the dependency cycle".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                })
                .collect::<Vec<_>>(),

            AddressOfSelf { name, span } => vec![ErrorLabel {
                message: format!(
                    "this argument refers to prediate `{name}` in which this intrinsic is used"
                ),
                span: span.clone(),
                color: Color::Red,
            }],

            PredicateNameNotFound { span, .. } => vec![ErrorLabel {
                message: "argument to `__address_of` must be a valid predicate name".to_string(),
                span: span.clone(),
                color: Color::Red,
            }],

            MatchExprNotUnion { found_ty, span } => vec![ErrorLabel {
                message: format!("matched expression must be a union, found `{found_ty}`"),
                span: span.clone(),
                color: Color::Red,
            }],

            MatchVariantUnknown {
                variant,
                union_name,
                span,
                ..
            } => vec![ErrorLabel {
                message: format!("invalid variant name `{variant}` for union `{union_name}`"),
                span: span.clone(),
                color: Color::Red,
            }],

            MatchBranchTypeMismatch {
                expected_ty,
                found_ty,
                span,
            } => vec![ErrorLabel {
                message: format!("expecting type `{expected_ty}`, found `{found_ty}`"),
                span: span.clone(),
                color: Color::Red,
            }],

            MatchBranchReused { name, span } => vec![ErrorLabel {
                message: format!("match variant `{name}` has previously been bound"),
                span: span.clone(),
                color: Color::Red,
            }],

            MatchBranchMissing {
                union_name, span, ..
            } => vec![ErrorLabel {
                message: format!("not all variants for union `{union_name}` are covered by match"),
                span: span.clone(),
                color: Color::Red,
            }],

            UnknownUnion { name, span } => vec![ErrorLabel {
                message: format!("union declaration for `{name}` not found"),
                span: span.clone(),
                color: Color::Red,
            }],

            UnknownUnionVariant { name, span, .. } => vec![ErrorLabel {
                message: format!("union variant `{name}` not found"),
                span: span.clone(),
                color: Color::Red,
            }],

            SuperfluousUnionExprValue { name, span } => vec![ErrorLabel {
                message: format!("union variant `{name}` should not bind a value"),
                span: span.clone(),
                color: Color::Red,
            }],

            MissingUnionExprValue {
                name,
                variant_ty,
                span,
            } => vec![ErrorLabel {
                message: format!("union variant `{name}` requires a value of type `{variant_ty}`"),
                span: span.clone(),
                color: Color::Red,
            }],

            UnionVariantTypeMismatch {
                found_ty,
                expected_ty,
                span,
            } => vec![ErrorLabel {
                message: format!("expecting type `{expected_ty}`, found `{found_ty}`"),
                span: span.clone(),
                color: Color::Red,
            }],

            InvalidStorageAccess { span } => vec![ErrorLabel {
                message: "storage cannot be accessed in this position".to_string(),
                span: span.clone(),
                color: Color::Red,
            }],

            IdenticalPredicates {
                original_span,
                span,
                ..
            } => vec![
                ErrorLabel {
                    message: "original predicate declaration here".to_string(),
                    span: original_span.clone(),
                    color: Color::Red,
                },
                ErrorLabel {
                    message: "predicate with identical bytecode here".to_string(),
                    span: span.clone(),
                    color: Color::Blue,
                },
            ],

            InvalidMapRangeType { found_ty, span } => vec![ErrorLabel {
                message: format!("expecting either array or range, found `{found_ty}`"),
                span: span.clone(),
                color: Color::Red,
            }],

            InvalidConst { span } => vec![ErrorLabel {
                message: "non-constant value".to_string(),
                span: span.clone(),
                color: Color::Red,
            }],

            FileIO { .. } => Vec::new(),
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
                path_union,
                ..
            } => Some(format!(
                "one of the modules `{path_mod}` or `{path_union}` must exist",
            )),

            MacroDeclClash { name, .. } => Some(format!(
                "it is valid to have multiple macros named `{name}` \
                but they must have differing parameter lists"
            )),

            MacroCallMismatch {
                arg_count,
                param_counts_descr,
                ..
            } => Some(format!(
                "the valid number of arguments must be {param_counts_descr} \
                        but this call passes {arg_count} arguments"
            )),

            MacroRecursion { .. } => Some(
                "a macro called recursively with the same number of arguments \
                    will cause a non-terminating loop during expansion"
                    .to_string(),
            ),

            MacroSpliceArrayUnknownSize { .. } => Some(
                "macro array splicing is currently limited to immediate integer sizes or \
                enumeration unions"
                    .to_string(),
            ),

            MacroCallWasNotExpression { .. } => Some(
                "macros which contain only declarations may only be used at the top level of \
                a predicate and not as an expression"
                    .to_string(),
            ),

            DuplicateGeneratorIndex { name, gen_kind, .. } => Some(format!(
                "`{gen_kind}` index `{name}` must be declared only once in this scope"
            )),

            InvalidGeneratorIndexBound { gen_kind, .. } => Some(format!(
                "`{gen_kind}` index bound must be an integer literal"
            )),

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

            InvalidArrayRangeType { found_ty, .. } => {
                Some(format!("found range type `{found_ty}`"))
            }

            StorageMapAccessWithWrongType { found_ty, .. } => {
                Some(format!("found access using type `{found_ty}`"))
            }

            ParamHasStorageType { ty, nested_ty, .. } => {
                if ty != nested_ty {
                    Some(format!(
                        "type of parameter depends on the storage type `{nested_ty}`"
                    ))
                } else {
                    None
                }
            }

            VarHasStorageType { ty, nested_ty, .. } => {
                if ty != nested_ty {
                    Some(format!(
                        "type of local variable depends on the storage type `{nested_ty}`"
                    ))
                } else {
                    None
                }
            }

            DependencyCycle { .. } => Some(
                "dependency between predicates is typically created via \
                     predicate instances"
                    .to_string(),
            ),

            InvalidStorageAccess { .. } => Some(
                "storage can only be accessed in the initializer of a `let` declaration"
                    .to_string(),
            ),

            IdenticalPredicates { .. } => Some(
                "two predicates in a contract cannot have the exact same (optimized) bytecode"
                    .to_string(),
            ),

            FileIO { .. }
            | MacroNotFound { .. }
            | MacroUndefinedParam { .. }
            | SymbolNotFound { .. }
            | StorageSymbolNotFound { .. }
            | MissingStorageBlock { .. }
            | InvalidNextStateAccess { .. }
            | MissingInterface { .. }
            | MissingPredicate { .. }
            | SelfReferencialPredicate { .. }
            | AddressExpressionTypeError { .. }
            | NonConstArrayLength { .. }
            | InvalidConstArrayLength { .. }
            | NonConstArrayIndex { .. }
            | InvalidConstArrayIndex { .. }
            | ArrayIndexOutOfBounds { .. }
            | CannotIndexIntoValue { .. }
            | MacroMultiplePacks { .. }
            | MacroUnknownPack { .. }
            | MacroNonUniqueParamCounts { .. }
            | MacroUnrecognizedSpliceVar { .. }
            | MacroSpliceVarNotArray { .. }
            | UnknownType { .. }
            | UndefinedType { .. }
            | UninferrableType { .. }
            | NonBoolConditional { .. }
            | SelectBranchesTypeMismatch { .. }
            | ConstraintExpressionTypeError { .. }
            | OperatorTypeError { .. }
            | OperatorInvalidType { .. }
            | InitTypeError { .. }
            | VarInitTypeError { .. }
            | IndexExprNonIndexable { .. }
            | TupleAccessNonTuple { .. }
            | EmptyArrayExpression { .. }
            | ExprRecursion { .. }
            | BadCastTo { .. }
            | BadCastFrom { .. }
            | RangeTypesMismatch { .. }
            | RangeTypesNonNumeric { .. }
            | InExprTypesMismatch { .. }
            | InExprTypesArrayMismatch { .. }
            | NonIntGeneratorRange { .. }
            | NonBoolGeneratorCondition { .. }
            | NonBoolGeneratorBody { .. }
            | UnexpectedIntrinsicArgCount { .. }
            | UnexpectedPredicateArgCount { .. }
            | MismatchedIntrinsicArgType { .. }
            | MismatchedPredicateArgType { .. }
            | RecursiveNewType { .. }
            | AddressOfSelf { .. }
            | TypeNotAllowedInStorage { .. }
            | PredicateNameNotFound { .. }
            | MatchExprNotUnion { .. }
            | MatchVariantUnknown { .. }
            | MatchBranchTypeMismatch { .. }
            | MatchBranchReused { .. }
            | MatchBranchMissing { .. }
            | UnknownUnion { .. }
            | UnknownUnionVariant { .. }
            | SuperfluousUnionExprValue { .. }
            | MissingUnionExprValue { .. }
            | UnionVariantTypeMismatch { .. }
            | VarsDependencyCycle { .. }
            | InvalidMapRangeType { .. }
            | InvalidConst { .. } => None,
        }
    }

    fn code(&self) -> Option<String> {
        None
    }

    fn help(&self) -> Option<String> {
        use CompileError::*;
        match self {
            SymbolNotFound { union_names, .. } if !union_names.is_empty() => Some(format!(
                "this symbol is a variant of union{} {} and may need a fully qualified path",
                if union_names.len() > 1 { "s" } else { "" },
                pretty_join_strings(union_names),
            )),

            MatchVariantUnknown {
                actual_variants, ..
            } if !actual_variants.is_empty() => Some(format!(
                "valid variant name{} {} {}",
                if actual_variants.len() > 1 { "s" } else { "" },
                if actual_variants.len() > 1 {
                    "are"
                } else {
                    "is"
                },
                pretty_join_strings(actual_variants),
            )),

            MacroCallMismatch {
                name, suggestion, ..
            } => suggestion.clone().or(Some(format!(
                "a macro named `{name}` found with a different signature"
            ))),

            BadCastTo { .. } => {
                if cfg!(feature = "experimental-types") {
                    Some("casts may only be made to `int` or `real`".to_string())
                } else {
                    Some("casts may only be made to `int`".to_string())
                }
            }

            BadCastFrom { .. } => {
                if cfg!(feature = "experimental-types") {
                    Some(
                    "casts may only be made from `bool`s,`int`s, and enumeration unions to `int`, \
                    or from `int`s, `real`s, and enumeration unions to `real`"
                        .to_string()
                    )
                } else {
                    Some(
                    "casts may only be made from `bool`s,`int`s, and enumeration unions to `int`"
                        .to_string()
                    )
                }
            }

            UnknownUnionVariant { valid_variants, .. } if !valid_variants.is_empty() => {
                Some(format!(
                    "valid variant name{} {} {}",
                    if valid_variants.len() > 1 { "s" } else { "" },
                    if valid_variants.len() > 1 {
                        "are"
                    } else {
                        "is"
                    },
                    pretty_join_strings(valid_variants),
                ))
            }

            MatchBranchMissing {
                missing_variants, ..
            } if !missing_variants.is_empty() => Some(format!(
                "branches and/or bindings are required for variant{} {}",
                if missing_variants.len() > 1 { "s" } else { "" },
                pretty_join_strings(missing_variants),
            )),

            _ => None,
        }
    }
}

fn pretty_join_strings(labels: &[String]) -> String {
    labels
        .iter()
        .enumerate()
        .map(|(idx, label)| {
            if idx + 2 == labels.len() {
                // 2nd last
                format!("`{label}` and ")
            } else if idx + 1 == labels.len() {
                // last
                format!("`{label}`")
            } else {
                // otherwise...
                format!("`{label}`, ")
            }
        })
        .collect::<Vec<_>>()
        .join("")
}

fn generate_type_error_labels(
    what: &str,
    found_ty: &str,
    expected_ty: &str,
    found_span: &Span,
    expected_span: &Option<Span>,
) -> Vec<ErrorLabel> {
    let mut labels = Vec::default();
    let mut expected_label_color = Color::Red;

    if found_ty != "Unknown" && found_ty != "Error" {
        labels.push(ErrorLabel {
            message: format!("{what} has unexpected type `{found_ty}`"),
            span: found_span.clone(),
            color: Color::Red,
        });

        // Make this label red and the next one blue.
        expected_label_color = Color::Blue;
    };

    if let Some(span) = expected_span {
        if expected_ty != "Unknown" && expected_ty != "Error" {
            labels.push(ErrorLabel {
                message: format!("expecting type `{expected_ty}`"),
                span: span.clone(),
                color: expected_label_color,
            });
        }
    }

    labels
}

impl Spanned for CompileError {
    fn span(&self) -> &Span {
        use CompileError::*;
        match self {
            FileIO { span, .. }
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
            | MacroUnrecognizedSpliceVar { span, .. }
            | MacroSpliceVarNotArray { span, .. }
            | MacroSpliceArrayUnknownSize { span, .. }
            | MacroCallWasNotExpression { span }
            | DuplicateGeneratorIndex { span, .. }
            | InvalidGeneratorIndexBound { span, .. }
            | NonIntGeneratorRange { span, .. }
            | NonBoolGeneratorCondition { span, .. }
            | NonBoolGeneratorBody { span, .. }
            | SymbolNotFound { span, .. }
            | StorageSymbolNotFound { span, .. }
            | InvalidNextStateAccess { span, .. }
            | MissingStorageBlock { span, .. }
            | MissingInterface { span, .. }
            | MissingPredicate { span, .. }
            | SelfReferencialPredicate { span, .. }
            | NonConstArrayIndex { span }
            | InvalidConstArrayLength { span }
            | NonConstArrayLength { span }
            | InvalidConstArrayIndex { span }
            | ArrayIndexOutOfBounds { span }
            | CannotIndexIntoValue { span, .. }
            | UnknownType { span }
            | UndefinedType { span }
            | UninferrableType { span }
            | NonBoolConditional { span, .. }
            | IndexExprNonIndexable { span, .. }
            | ArrayAccessWithWrongType { span, .. }
            | InvalidArrayRangeType { span, .. }
            | ParamHasStorageType { span, .. }
            | VarHasStorageType { span, .. }
            | TypeNotAllowedInStorage { span, .. }
            | StorageMapAccessWithWrongType { span, .. }
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
            | RangeTypesMismatch { span, .. }
            | RangeTypesNonNumeric { span, .. }
            | InExprTypesMismatch { span, .. }
            | InExprTypesArrayMismatch { span, .. }
            | UnexpectedIntrinsicArgCount { span, .. }
            | UnexpectedPredicateArgCount { span, .. }
            | MismatchedIntrinsicArgType { arg_span: span, .. }
            | MismatchedPredicateArgType { arg_span: span, .. }
            | RecursiveNewType { use_span: span, .. }
            | AddressOfSelf { span, .. }
            | PredicateNameNotFound { span, .. }
            | MatchExprNotUnion { span, .. }
            | MatchVariantUnknown { span, .. }
            | MatchBranchTypeMismatch { span, .. }
            | MatchBranchReused { span, .. }
            | MatchBranchMissing { span, .. }
            | UnknownUnion { span, .. }
            | UnknownUnionVariant { span, .. }
            | SuperfluousUnionExprValue { span, .. }
            | MissingUnionExprValue { span, .. }
            | UnionVariantTypeMismatch { span, .. }
            | OperatorInvalidType { span, .. }
            | InvalidStorageAccess { span, .. }
            | IdenticalPredicates { span, .. }
            | InvalidMapRangeType { span, .. }
            | InvalidConst { span, .. } => span,

            DependencyCycle { spans } => &spans[0],
            VarsDependencyCycle { spans } => &spans[0],

            SelectBranchesTypeMismatch { large_err }
            | OperatorTypeError { large_err, .. }
            | VarInitTypeError { large_err, .. }
            | ConstraintExpressionTypeError { large_err, .. }
            | AddressExpressionTypeError { large_err, .. }
            | InitTypeError { large_err, .. } => match large_err.as_ref() {
                LargeTypeError::SelectBranchesTypeMismatch { span, .. }
                | LargeTypeError::OperatorTypeError { span, .. }
                | LargeTypeError::VarInitTypeError { span, .. }
                | LargeTypeError::ConstraintExpressionTypeError { span, .. }
                | LargeTypeError::AddressExpressionTypeError { span, .. }
                | LargeTypeError::InitTypeError {
                    init_span: span, ..
                } => span,
            },
        }
    }
}
