use crate::{
    error::{CompileError, Error, ErrorEmitted},
    expr::{Expr, GeneratorKind},
    predicate::{ExprKey, Handler, Predicate, Program, SolveFunc},
    span::empty_span,
    types::Type,
};

pub(crate) fn validate(handler: &Handler, program: &mut Program) -> Result<(), ErrorEmitted> {
    program.preds.values().for_each(|pred| {
        check_constraints(pred, handler);
        check_vars(pred, handler);
        check_states(pred, handler);
        check_ifs(pred, handler);
        check_directive(pred, handler);
    });

    Ok(())
}

fn check_vars(pred: &Predicate, handler: &Handler) {
    for (var_key, var) in pred.vars() {
        if var_key.get_ty(pred).is_unknown() {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                msg:
                    "final predicate var_types slotmap is missing corresponding key from vars slotmap",
                span: var.span.clone(),
            }});
        }
    }
}

fn check_states(pred: &Predicate, handler: &Handler) {
    for (state_key, state) in pred.states() {
        if state_key.get_ty(pred).is_unknown() {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                msg:
                    "final predicate state_types slotmap is missing corresponding key from states slotmap",
                span: state.span.clone(),
            }});
        }
    }
}

fn check_ifs(pred: &Predicate, handler: &Handler) {
    if !pred.if_decls.is_empty() {
        handler.emit_err(Error::Compile {
            error: CompileError::Internal {
                msg: "final predicate contains if declarations",
                span: pred
                    .if_decls
                    .last()
                    .expect("guaranteed to exist")
                    .span
                    .clone(),
            },
        });
    }
}

fn check_directive(pred: &Predicate, handler: &Handler) {
    if pred.directives.len() > 1 {
        handler.emit_err(Error::Compile {
            error: CompileError::Internal {
                msg: "final predicate contains more than one `solve` directive",
                span: pred
                    .directives
                    .last()
                    .expect("guaranteed to exist")
                    .1
                    .clone(),
            },
        });
    }

    pred.directives
        .iter()
        .for_each(|(solve_func, _)| match solve_func {
            SolveFunc::Minimize(expr_key) | SolveFunc::Maximize(expr_key) => {
                let _ = check_expr(expr_key, handler, pred);
            }
            SolveFunc::Satisfy => {}
        })
}

fn check_constraints(pred: &Predicate, handler: &Handler) {
    for expr_key in pred.exprs() {
        let _ = check_expr(&expr_key, handler, pred);
    }
}

fn check_expr(expr_key: &ExprKey, handler: &Handler, pred: &Predicate) -> Result<(), ErrorEmitted> {
    macro_rules! emit_illegal_type_error {
        ($handler: expr, $span: expr, $type_str: literal, $slotmap_str: literal) => {
            $handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: concat!(
                        $type_str,
                        " present in final predicate ",
                        $slotmap_str,
                        " slotmap"
                    ),
                    span: $span.clone(),
                },
            })
        };
    }

    let expr_type = expr_key.get_ty(pred);
    if expr_type.is_unknown() {
        handler.emit_err(Error::Compile {
            error: CompileError::Internal {
                msg: "Unknown expr type found invalid predicate expr_types slotmap key",
                span: empty_span(),
            },
        });
    }

    let expr = expr_key.try_get(pred).ok_or_else(|| {
        handler.emit_err(Error::Compile {
            error: CompileError::Internal {
                msg: "invalid predicate exprs slotmap key",
                span: empty_span(),
            },
        })
    })?;

    // validate the expr_type is legal
    match expr_type {
        Type::Error(span) => {
            emit_illegal_type_error!(handler, span, "error type", "expr_types");
        }
        Type::Unknown(span) => {
            emit_illegal_type_error!(handler, span, "unknown type", "expr_types");
        }
        Type::Custom { span, .. } => {
            emit_illegal_type_error!(handler, span, "custom type", "expr_types");
        }
        Type::Alias { span, .. } => {
            emit_illegal_type_error!(handler, span, "type alias", "expr_types");
        }
        Type::Array { .. } | Type::Tuple { .. } | Type::Primitive { .. } | Type::Map { .. } => {}
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
        Expr::Immediate { .. }
        | Expr::Array { .. }
        | Expr::Tuple { .. }
        | Expr::PathByKey(..)
        | Expr::PathByName(..)
        | Expr::StorageAccess(..)
        | Expr::UnaryOp { .. }
        | Expr::BinaryOp { .. }
        | Expr::IntrinsicCall { .. }
        | Expr::Select { .. }
        | Expr::Cast { .. }
        | Expr::TupleFieldAccess { .. }
        | Expr::Index { .. }
        | Expr::ExternalStorageAccess { .. } => Ok(()),
    }
}

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[cfg(test)]
fn run_test(src: &str) -> String {
    use crate::error;
    let (mut program, handler) = run_without_transforms(src);
    let _ = validate(&handler, &mut program);
    error::Errors(handler.consume()).to_string()
}

#[cfg(test)]
fn run_without_transforms(src: &str) -> (Program, Handler) {
    let handler = Handler::default();
    let parsed_source = run_parser(src, &handler);
    let type_checked_source = parsed_source
        .type_check(&handler)
        .expect("Failed to type check");
    (type_checked_source, handler)
}

#[cfg(test)]
fn run_parser(src: &str, handler: &Handler) -> Program {
    use crate::{parser::pint_parser, predicate::ProgramKind, span};
    use std::collections::BTreeMap;

    let parser = pint_parser::PintParser::new();
    let mut current_pred = Program::ROOT_PRED_NAME.to_string();
    let filepath = std::rc::Rc::from(std::path::Path::new("test"));
    let mut program = Program {
        kind: ProgramKind::Stateless,
        preds: BTreeMap::from([(Program::ROOT_PRED_NAME.to_string(), Predicate::default())]),
        consts: fxhash::FxHashMap::default(),
    };

    parser
        .parse(
            &mut crate::parser::ParserContext {
                mod_path: &[],
                mod_prefix: "",
                local_scope: None,
                program: &mut program,
                current_pred: &mut current_pred,
                macros: &mut Vec::new(),
                macro_calls: &mut BTreeMap::from([(
                    Program::ROOT_PRED_NAME.to_string(),
                    slotmap::SecondaryMap::new(),
                )]),
                span_from: &|_, _| span::empty_span(),
                use_paths: &mut Vec::new(),
                next_paths: &mut Vec::new(),
            },
            handler,
            crate::lexer::Lexer::new(src, &filepath, &[]),
        )
        .expect("Failed to parse test case.");

    program
}

#[test]
fn expr_types() {
    // array
    let src = "var a = [1, 2, 3];";
    check(&run_test(src), expect_test::expect![""]);
    // tuple
    let src = "var t = { x: 5, 3 };";
    check(&run_test(src), expect_test::expect![""]);
    // custom / enum
    let src = "enum MyEnum = Variant1 | Variant2;
    var x = MyEnum;";
    check(
        &run_test(src),
        expect_test::expect![[r#"
        compiler internal error: custom type present in final predicate expr_types slotmap
        compiler internal error: custom type present in final predicate expr_types slotmap"#]],
    );
    // type alias
    let src = "type MyAliasInt = int;
    var x: MyAliasInt = 3;";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: type alias present in final predicate expr_types slotmap"#
        ]],
    )
}

#[test]
fn exprs() {
    // array and array field access
    let src = "var x: bool = 5 in [3, 4, 5];";
    check(
        &run_test(src),
        expect_test::expect![
            "compiler internal error: in expression present in final predicate exprs slotmap"
        ],
    );
    // forall
    let src = "var k: int;
    constraint forall i in 0..3, j in 0..3 where !(i >= j), i - 1 >= 0 && j > 0 { !(i - j < k) };";
    check(
        &run_test(src),
        expect_test::expect![[r#"
            compiler internal error: forall generator present in final predicate exprs slotmap
            compiler internal error: range present in final predicate exprs slotmap
            compiler internal error: range present in final predicate exprs slotmap"#]],
    );
    // exists
    let src = "var a: int[2][2];
    constraint exists i in 0..1, j in 0..1 {
        a[i][j] == 70
    };";
    check(
        &run_test(src),
        expect_test::expect![[r#"
            compiler internal error: exists generator present in final predicate exprs slotmap
            compiler internal error: range present in final predicate exprs slotmap
            compiler internal error: range present in final predicate exprs slotmap"#]],
    );
}

#[test]
fn states() {
    use crate::error;
    use crate::predicate::State;

    let src = "var a = 1;";
    let (mut program, handler) = run_without_transforms(src);
    program.preds.iter_mut().for_each(|(_, pred)| {
        let dummy_expr_key = pred
            .exprs
            .insert(Expr::Error(empty_span()), Type::Unknown(empty_span()));
        let dummy_state = State {
            name: "test".to_owned(),
            expr: dummy_expr_key,
            span: empty_span(),
        };
        pred.states.insert(dummy_state, Type::Unknown(empty_span()));
    });
    let _ = validate(&handler, &mut program);
    check(
        &error::Errors(handler.consume()).to_string(),
        expect_test::expect![[r#"
            compiler internal error: Unknown expr type found invalid predicate expr_types slotmap key
            compiler internal error: unknown type present in final predicate expr_types slotmap
            compiler internal error: error expression present in final predicate exprs slotmap
            compiler internal error: final predicate state_types slotmap is missing corresponding key from states slotmap"#]],
    );
}

#[test]
fn vars() {
    use crate::error;
    use crate::predicate::Var;

    let src = "var a = 1;";
    let (mut program, handler) = run_without_transforms(src);
    program.preds.iter_mut().for_each(|(_, pred)| {
        pred.vars.insert(
            Var {
                name: "test".to_owned(),
                is_pub: false,
                span: empty_span(),
            },
            Type::Unknown(empty_span()),
        );
    });
    let _ = validate(&handler, &mut program);
    check(
        &error::Errors(handler.consume()).to_string(),
        expect_test::expect!["compiler internal error: final predicate var_types slotmap is missing corresponding key from vars slotmap"],
    );
}

#[test]
fn if_decls() {
    use crate::error;

    let src = "if true { constraint true; } solve satisfy;";
    let (mut program, handler) = run_without_transforms(src);
    let _ = validate(&handler, &mut program);
    check(
        &error::Errors(handler.consume()).to_string(),
        expect_test::expect!["compiler internal error: final predicate contains if declarations"],
    );
}

#[test]
fn directives() {
    use crate::error;

    let src = "var a = 1;";
    let (mut program, handler) = run_without_transforms(src);
    program.preds.iter_mut().for_each(|(_, pred)| {
        let solve_directive = (SolveFunc::Satisfy, empty_span());
        pred.directives.push(solve_directive);

        let dummy_expr_key = pred.exprs.insert(
            Expr::Error(empty_span()),
            Type::Custom {
                path: "::b".to_owned(),
                span: empty_span(),
            },
        );

        let maximize_directive = (SolveFunc::Maximize(dummy_expr_key), empty_span());
        pred.directives.push(maximize_directive);

        let minimize_directive = (SolveFunc::Minimize(dummy_expr_key), empty_span());
        pred.directives.push(minimize_directive);
    });
    let _ = validate(&handler, &mut program);
    check(
        &error::Errors(handler.consume()).to_string(),
        expect_test::expect![[r#"
            compiler internal error: custom type present in final predicate expr_types slotmap
            compiler internal error: error expression present in final predicate exprs slotmap
            compiler internal error: custom type present in final predicate expr_types slotmap
            compiler internal error: error expression present in final predicate exprs slotmap
            compiler internal error: final predicate contains more than one `solve` directive
            compiler internal error: custom type present in final predicate expr_types slotmap
            compiler internal error: error expression present in final predicate exprs slotmap
            compiler internal error: custom type present in final predicate expr_types slotmap
            compiler internal error: error expression present in final predicate exprs slotmap"#]],
    );
}
