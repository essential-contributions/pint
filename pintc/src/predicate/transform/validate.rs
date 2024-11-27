use crate::{
    error::ErrorEmitted,
    expr::{Expr, GeneratorKind},
    predicate::{Contract, ExprKey, Handler, PredKey, Predicate, UnionDecl},
    span::empty_span,
    types::Type,
};

pub(crate) fn validate(handler: &Handler, contract: &mut Contract) {
    for (pred_key, pred) in contract.preds.iter() {
        check_constraints(contract, pred_key, handler);
        check_params(pred, handler);
        check_variables(pred, handler);
        check_ifs_and_matches(pred, handler);
    }
}

fn check_params(pred: &Predicate, handler: &Handler) {
    for param in &pred.params {
        if param.ty.is_unknown() {
            handler.emit_internal_err(
                "final predicate var_types slotmap is missing corresponding key from vars slotmap",
                param.span.clone(),
            );
        }
    }
}

fn check_variables(pred: &Predicate, handler: &Handler) {
    for (variable_key, variable) in pred.variables() {
        if variable_key.get_ty(pred).is_unknown() {
            handler.emit_internal_err(
                "final predicate variable_types slotmap is missing corresponding key from \
                    variables slotmap",
                variable.span.clone(),
            );
        }
    }
}

fn check_ifs_and_matches(pred: &Predicate, handler: &Handler) {
    if !pred.if_decls.is_empty() {
        handler.emit_internal_err(
            "final predicate contains if declarations",
            pred.if_decls[0].span.clone(),
        );
    }

    if !pred.match_decls.is_empty() {
        handler.emit_internal_err(
            "final predicate contains match declarations",
            pred.match_decls[0].span.clone(),
        );
    }
}

fn check_constraints(contract: &Contract, pred_key: PredKey, handler: &Handler) {
    for expr_key in contract.exprs(pred_key) {
        let _ = check_expr(&expr_key, handler, contract);
    }
}

fn check_expr(
    expr_key: &ExprKey,
    handler: &Handler,
    contract: &Contract,
) -> Result<(), ErrorEmitted> {
    macro_rules! emit_illegal_type_error {
        ($handler: expr, $span: expr, $type_str: literal, $slotmap_str: literal) => {
            $handler.emit_internal_err(
                concat!(
                    $type_str,
                    " present in final predicate ",
                    $slotmap_str,
                    " slotmap"
                ),
                $span.clone(),
            )
        };
    }

    let expr_type = expr_key.get_ty(contract);
    if expr_type.is_unknown() {
        handler.emit_internal_err(
            "Unknown expr type found invalid predicate expr_types slotmap key",
            empty_span(),
        );
    }

    let expr = expr_key.try_get(contract).ok_or_else(|| {
        handler.emit_internal_err("invalid predicate exprs slotmap key", empty_span())
    })?;

    // validate the expr_type is legal
    match expr_type {
        Type::Error(span) => {
            emit_illegal_type_error!(handler, span, "error type", "expr_types");
        }
        Type::Unknown(span) => {
            emit_illegal_type_error!(handler, span, "unknown type", "expr_types");
        }
        Type::Any(span) => {
            emit_illegal_type_error!(handler, span, "any type", "expr_types");
        }
        Type::Custom { name, span, .. } => {
            // TODO: unclear how to test this. We will refactor custom types soon anyways.
            if !contract.unions.values().any(
                |UnionDecl {
                     name: union_name, ..
                 }| &union_name.name == name,
            ) {
                emit_illegal_type_error!(handler, span, "custom type", "expr_types");
            }
        }
        Type::Alias { span, .. } => {
            emit_illegal_type_error!(handler, span, "type alias", "expr_types");
        }
        Type::Array { .. }
        | Type::Tuple { .. }
        | Type::Primitive { .. }
        | Type::Map { .. }
        | Type::Vector { .. }
        | Type::Union { .. } => {}
    }

    // then check the expr variant and make sure legal
    match expr {
        Expr::Error(span) => Err(emit_illegal_type_error!(
            handler,
            span,
            "error expression",
            "exprs"
        )),

        Expr::MacroCall { span, .. } => Err(emit_illegal_type_error!(
            handler,
            span,
            "macro call",
            "exprs"
        )),

        Expr::In { span, .. } => Err(emit_illegal_type_error!(
            handler,
            span,
            "in expression",
            "exprs"
        )),

        Expr::Range { span, .. } => Err(emit_illegal_type_error!(handler, span, "range", "exprs")),

        Expr::Generator { kind, span, .. } => match kind {
            GeneratorKind::ForAll => Err(emit_illegal_type_error!(
                handler,
                span,
                "forall generator",
                "exprs"
            )),
            GeneratorKind::Exists => Err(emit_illegal_type_error!(
                handler,
                span,
                "exists generator",
                "exprs"
            )),
        },

        Expr::Match { span, .. } => Err(emit_illegal_type_error!(
            handler,
            span,
            "match expression",
            "exprs"
        )),

        Expr::Immediate { .. }
        | Expr::Array { .. }
        | Expr::Tuple { .. }
        | Expr::UnionVariant { .. }
        | Expr::Path(..)
        | Expr::LocalStorageAccess { .. }
        | Expr::ExternalStorageAccess { .. }
        | Expr::UnaryOp { .. }
        | Expr::BinaryOp { .. }
        | Expr::IntrinsicCall { .. }
        | Expr::LocalPredicateCall { .. }
        | Expr::ExternalPredicateCall { .. }
        | Expr::Select { .. }
        | Expr::Cast { .. }
        | Expr::TupleFieldAccess { .. }
        | Expr::Index { .. }
        | Expr::Map { .. }
        | Expr::UnionTag { .. }
        | Expr::UnionValue { .. } => Ok(()),
    }
}

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[cfg(test)]
fn run_test(src: &str) -> String {
    use crate::error;
    let (mut contract, handler) = run_without_transforms(src);
    validate(&handler, &mut contract);
    error::Errors(handler.consume().0).to_string()
}

#[cfg(test)]
fn run_without_transforms(src: &str) -> (Contract, Handler) {
    let handler = Handler::default();
    let parsed_source = run_parser(src, &handler);
    let type_checked_source = parsed_source
        .type_check(&handler)
        .expect("Failed to type check");
    (type_checked_source, handler)
}

#[cfg(test)]
fn run_parser(src: &str, handler: &Handler) -> Contract {
    use crate::{
        lexer,
        parser::{self, pint_parser},
        span,
    };
    use std::collections::BTreeMap;

    let parser = pint_parser::PintParser::new();
    let filepath = std::sync::Arc::from(std::path::Path::new("test"));
    let mut contract = Contract::default();

    parser
        .parse(
            &mut parser::ParserContext {
                mod_path: &[],
                mod_prefix: "",
                local_scope: None,
                contract: &mut contract,
                current_pred_key: None,
                macros: &mut Vec::new(),
                macro_calls: &mut BTreeMap::default(),
                span_from: &|_, _| span::empty_span(),
                use_paths: &mut Vec::new(),
                next_paths: &mut Vec::new(),
                experimental_types: true,
            },
            handler,
            lexer::Lexer::new(src, &filepath, &[]),
        )
        .expect("Failed to parse test case.");

    contract
}

#[test]
fn expr_types() {
    // array
    let src = "predicate test(a: int[3]) { constraint a == [1, 2, 3]; }";
    check(&run_test(src), expect_test::expect![""]);
    // tuple
    let src = "predicate test(t: {x: int, int}) { constraint t == { x: 5, 3 }; }";
    check(&run_test(src), expect_test::expect![""]);

    // type alias
    let src = r#"
type MyAliasInt = int;
predicate test(x: MyAliasInt) { constraint x == 3; }
"#;

    // Do this manually here because we have to copy the new type into the predicate.
    let handler = Handler::default();
    let mut contract = run_parser(src, &handler)
        .type_check(&handler)
        .expect("Failed to type check");
    validate(&handler, &mut contract);

    check(
        &crate::error::Errors(handler.consume().0).to_string(),
        expect_test::expect![[
            r#"compiler internal error: type alias present in final predicate expr_types slotmap"#
        ]],
    )
}

#[test]
fn exprs() {
    // array and array field access
    let src = "predicate test(x: bool) { constraint x == 5 in [3, 4, 5]; }";
    check(
        &run_test(src),
        expect_test::expect![
            "compiler internal error: in expression present in final predicate exprs slotmap"
        ],
    );
    // forall
    let src = "predicate test(k: int) { \
                   constraint forall i in 0..3, j in 0..3 where !(i >= j), i - 1 >= 0 && j > 0 { \
                       !(i - j < k) }; \
                   }";
    check(
        &run_test(src),
        expect_test::expect![
            "compiler internal error: forall generator present in final predicate exprs slotmap"
        ],
    );
    // exists
    let src = "predicate test(a: int[2][2]) {
    constraint exists i in 0..1, j in 0..1 {
        a[i][j] == 70
    };}";
    check(
        &run_test(src),
        expect_test::expect![
            "compiler internal error: exists generator present in final predicate exprs slotmap"
        ],
    );
}

#[test]
fn variables() {
    use crate::error;
    use crate::predicate::Variable;

    let src = "predicate test(a: int) { constraint a == 1; }";
    let (mut contract, handler) = run_without_transforms(src);
    contract.preds.iter_mut().for_each(|(_, pred)| {
        let dummy_expr_key = contract
            .exprs
            .insert(Expr::Error(empty_span()), Type::Unknown(empty_span()));
        let dummy_variable = Variable {
            name: "test".to_owned(),
            expr: dummy_expr_key,
            span: empty_span(),
        };
        pred.variables
            .insert(dummy_variable, Type::Unknown(empty_span()));
    });
    validate(&handler, &mut contract);
    check(
        &error::Errors(handler.consume().0).to_string(),
        expect_test::expect![
            "compiler internal error: \
                Unknown expr type found invalid predicate expr_types slotmap key"
        ],
    );
}

#[test]
fn vars() {
    use crate::{
        error,
        predicate::{Ident, Param},
    };

    let src = "predicate test(a: int) { constraint a == 1; }";
    let (mut contract, handler) = run_without_transforms(src);
    contract.preds.iter_mut().for_each(|(_, pred)| {
        pred.params.push(Param {
            name: Ident {
                name: "test".to_owned(),
                hygienic: false,
                span: empty_span(),
            },
            ty: Type::Unknown(empty_span()),
            span: empty_span(),
        });
    });
    validate(&handler, &mut contract);
    check(
        &error::Errors(handler.consume().0).to_string(),
        expect_test::expect![
            "compiler internal error: \
                final predicate var_types slotmap is missing corresponding key from vars slotmap"
        ],
    );
}

#[test]
fn if_decls() {
    use crate::error;

    let src = "predicate test() { if true { constraint true; } }";
    let (mut contract, handler) = run_without_transforms(src);
    validate(&handler, &mut contract);
    check(
        &error::Errors(handler.consume().0).to_string(),
        expect_test::expect!["compiler internal error: final predicate contains if declarations"],
    );
}
