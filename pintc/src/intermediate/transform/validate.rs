use crate::{
    error::{CompileError, Error, ErrorEmitted},
    expr::{Expr, GeneratorKind},
    intermediate::{ExprKey, Handler, IntermediateIntent, Program, SolveFunc},
    span::empty_span,
    types::Type,
};

pub(crate) fn validate(handler: &Handler, program: &mut Program) -> Result<(), ErrorEmitted> {
    program.iis.values().for_each(|ii| {
        check_constraints(ii, handler);
        check_vars(ii, handler);
        check_states(ii, handler);
        check_ifs(ii, handler);
        check_directive(ii, handler);
    });

    Ok(())
}

fn check_vars(ii: &IntermediateIntent, handler: &Handler) {
    for (var_key, var) in ii.vars() {
        if var_key.get_ty(ii).is_unknown() {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                msg:
                    "final intent var_types slotmap is missing corresponding key from vars slotmap",
                span: var.span.clone(),
            }});
        }
    }
}

fn check_states(ii: &IntermediateIntent, handler: &Handler) {
    for (state_key, state) in ii.states() {
        if state_key.get_ty(ii).is_unknown() {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                msg:
                    "final intent state_types slotmap is missing corresponding key from states slotmap",
                span: state.span.clone(),
            }});
        }
    }
}

fn check_ifs(ii: &IntermediateIntent, handler: &Handler) {
    if !ii.if_decls.is_empty() {
        handler.emit_err(Error::Compile {
            error: CompileError::Internal {
                msg: "final intent contains if declarations",
                span: ii
                    .if_decls
                    .last()
                    .expect("guaranteed to exist")
                    .span
                    .clone(),
            },
        });
    }
}

fn check_directive(ii: &IntermediateIntent, handler: &Handler) {
    if ii.directives.len() > 1 {
        handler.emit_err(Error::Compile {
            error: CompileError::Internal {
                msg: "final intent contains more than one `solve` directive",
                span: ii.directives.last().expect("guaranteed to exist").1.clone(),
            },
        });
    }

    ii.directives
        .iter()
        .for_each(|(solve_func, _)| match solve_func {
            SolveFunc::Minimize(expr_key) | SolveFunc::Maximize(expr_key) => {
                let _ = check_expr(expr_key, handler, ii);
            }
            SolveFunc::Satisfy => {}
        })
}

fn check_constraints(ii: &IntermediateIntent, handler: &Handler) {
    for expr_key in ii.exprs() {
        let _ = check_expr(&expr_key, handler, ii);
    }
}

fn check_expr(
    expr_key: &ExprKey,
    handler: &Handler,
    ii: &IntermediateIntent,
) -> Result<(), ErrorEmitted> {
    macro_rules! emit_illegal_type_error {
        ($handler: expr, $span: expr, $type_str: literal, $slotmap_str: literal) => {
            $handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: concat!(
                        $type_str,
                        " present in final intent ",
                        $slotmap_str,
                        " slotmap"
                    ),
                    span: $span.clone(),
                },
            })
        };
    }

    let expr_type = expr_key.get_ty(ii);
    if expr_type.is_unknown() {
        handler.emit_err(Error::Compile {
            error: CompileError::Internal {
                msg: "Unknown expr type foundinvalid intermediate intent expr_types slotmap key",
                span: empty_span(),
            },
        });
    }

    let expr = expr_key.try_get(ii).ok_or_else(|| {
        handler.emit_err(Error::Compile {
            error: CompileError::Internal {
                msg: "invalid intermediate intent exprs slotmap key",
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
        Expr::Index { expr, span, .. } => {
            if !expr.get_ty(ii).is_map() {
                Err(emit_illegal_type_error!(
                    handler,
                    span,
                    "array element access",
                    "exprs"
                ))
            } else {
                Ok(())
            }
        }
        Expr::TupleFieldAccess { span, .. } => {
            if !expr_is_for_storage(ii, expr) {
                Err(emit_illegal_type_error!(
                    handler,
                    span,
                    "tuple field access",
                    "exprs"
                ))
            } else {
                Ok(())
            }
        }
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
        Expr::Array { .. }
        | Expr::Tuple { .. }
        | Expr::Immediate { .. }
        | Expr::PathByKey(..)
        | Expr::PathByName(..)
        | Expr::StorageAccess(..)
        | Expr::UnaryOp { .. }
        | Expr::BinaryOp { .. }
        | Expr::IntrinsicCall { .. }
        | Expr::Select { .. }
        | Expr::Cast { .. }
        | Expr::ExternalStorageAccess { .. } => Ok(()),
    }
}

fn expr_is_for_storage(ii: &IntermediateIntent, expr: &Expr) -> bool {
    match expr {
        // Recurse for the tuple expr or index (possibly into a Map).
        Expr::TupleFieldAccess { tuple: expr, .. } | Expr::Index { expr, .. } => expr
            .try_get(ii)
            .map(|agg_expr| expr_is_for_storage(ii, agg_expr))
            .unwrap_or(false),

        Expr::StorageAccess(_, _) | Expr::ExternalStorageAccess { .. } => true,

        // In the future we'll add other 'illegal' aggregate expressions which will also need to be
        // handled.
        Expr::Error(_)
        | Expr::Immediate { .. }
        | Expr::PathByKey(_, _)
        | Expr::PathByName(_, _)
        | Expr::UnaryOp { .. }
        | Expr::BinaryOp { .. }
        | Expr::MacroCall { .. }
        | Expr::IntrinsicCall { .. }
        | Expr::Select { .. }
        | Expr::Array { .. }
        | Expr::Tuple { .. }
        | Expr::Cast { .. }
        | Expr::In { .. }
        | Expr::Range { .. }
        | Expr::Generator { .. } => false,
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
    use crate::intermediate::ProgramKind;
    use crate::parser::pint_parser;
    use crate::span;
    use std::collections::BTreeMap;

    let parser = pint_parser::PintParser::new();
    let mut current_ii = Program::ROOT_II_NAME.to_string();
    let filepath = std::rc::Rc::from(std::path::Path::new("test"));
    let mut program = Program {
        kind: ProgramKind::Stateless,
        iis: BTreeMap::from([(
            Program::ROOT_II_NAME.to_string(),
            IntermediateIntent::default(),
        )]),
    };

    parser
        .parse(
            &mut crate::parser::ParserContext {
                mod_path: &[],
                mod_prefix: "",
                local_scope: None,
                program: &mut program,
                current_ii: &mut current_ii,
                macros: &mut Vec::new(),
                macro_calls: &mut BTreeMap::from([(
                    Program::ROOT_II_NAME.to_string(),
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
    let src = "let a = [1, 2, 3];";
    check(&run_test(src), expect_test::expect![""]);
    // tuple
    let src = "let t = { x: 5, 3 };";
    check(&run_test(src), expect_test::expect![""]);
    // custom / enum
    let src = "enum MyEnum = Variant1 | Variant2;
    let x = MyEnum;";
    check(
        &run_test(src),
        expect_test::expect![[r#"
        compiler internal error: custom type present in final intent expr_types slotmap
        compiler internal error: custom type present in final intent expr_types slotmap"#]],
    );
    // type alias
    let src = "type MyAliasInt = int;
    let x: MyAliasInt = 3;";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: type alias present in final intent expr_types slotmap"#
        ]],
    )
}

#[test]
fn exprs() {
    // tuple and tuple field access
    let src = "let t = { y: 3, 2 };
    let x = t.1;";
    check(
        &run_test(src),
        expect_test::expect![
            "compiler internal error: tuple field access present in final intent exprs slotmap"
        ],
    );
    // array and array field access
    let src = "let a = [1, 2, 3];
    let b = a[1];";
    check(
        &run_test(src),
        expect_test::expect![
            "compiler internal error: array element access present in final intent exprs slotmap"
        ],
    );
    // <<disabled>> until if check is supported
    // if
    // let src = "let b: int;
    // let c = false;
    // constraint b < if c { 22 } else { 33 };";
    // check(
    //     &run_test(src),
    //     expect_test::expect![[
    //         r#"compiler internal error: if expression present in final intent exprs slotmap"#
    //     ]],
    // );
    // in
    let src = "let x: bool = 5 in [3, 4, 5];";
    check(
        &run_test(src),
        expect_test::expect![
            "compiler internal error: in expression present in final intent exprs slotmap"
        ],
    );
    // forall
    let src = "let k: int;
    constraint forall i in 0..3, j in 0..3 where !(i >= j), i - 1 >= 0 && j > 0 { !(i - j < k) };";
    check(
        &run_test(src),
        expect_test::expect![[r#"
            compiler internal error: forall generator present in final intent exprs slotmap
            compiler internal error: range present in final intent exprs slotmap
            compiler internal error: range present in final intent exprs slotmap"#]],
    );
    // exists
    let src = "let a: int[2][2];
    constraint exists i in 0..1, j in 0..1 {
        a[i][j] == 70
    };";
    check(
        &run_test(src),
        expect_test::expect![[r#"
            compiler internal error: exists generator present in final intent exprs slotmap
            compiler internal error: array element access present in final intent exprs slotmap
            compiler internal error: array element access present in final intent exprs slotmap
            compiler internal error: range present in final intent exprs slotmap
            compiler internal error: range present in final intent exprs slotmap"#]],
    );
}

#[test]
fn states() {
    use crate::error;
    use crate::intermediate::State;

    let src = "let a = 1;";
    let (mut program, handler) = run_without_transforms(src);
    program.iis.iter_mut().for_each(|(_, ii)| {
        let dummy_expr_key = ii
            .exprs
            .insert(Expr::Error(empty_span()), Type::Unknown(empty_span()));
        let dummy_state = State {
            name: "test".to_owned(),
            expr: dummy_expr_key,
            span: empty_span(),
        };
        ii.states.insert(dummy_state, Type::Unknown(empty_span()));
    });
    let _ = validate(&handler, &mut program);
    check(
        &error::Errors(handler.consume()).to_string(),
        expect_test::expect![[r#"
            compiler internal error: Unknown expr type foundinvalid intermediate intent expr_types slotmap key
            compiler internal error: unknown type present in final intent expr_types slotmap
            compiler internal error: error expression present in final intent exprs slotmap
            compiler internal error: final intent state_types slotmap is missing corresponding key from states slotmap"#]],
    );
}

#[test]
fn vars() {
    use crate::error;
    use crate::intermediate::Var;

    let src = "let a = 1;";
    let (mut program, handler) = run_without_transforms(src);
    program.iis.iter_mut().for_each(|(_, ii)| {
        ii.vars.insert(
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
        expect_test::expect!["compiler internal error: final intent var_types slotmap is missing corresponding key from vars slotmap"],
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
        expect_test::expect!["compiler internal error: final intent contains if declarations"],
    );
}

#[test]
fn directives() {
    use crate::error;

    let src = "let a = 1;";
    let (mut program, handler) = run_without_transforms(src);
    program.iis.iter_mut().for_each(|(_, ii)| {
        let solve_directive = (SolveFunc::Satisfy, empty_span());
        ii.directives.push(solve_directive);

        let dummy_expr_key = ii.exprs.insert(
            Expr::Error(empty_span()),
            Type::Custom {
                path: "::b".to_owned(),
                span: empty_span(),
            },
        );

        let maximize_directive = (SolveFunc::Maximize(dummy_expr_key), empty_span());
        ii.directives.push(maximize_directive);

        let minimize_directive = (SolveFunc::Minimize(dummy_expr_key), empty_span());
        ii.directives.push(minimize_directive);
    });
    let _ = validate(&handler, &mut program);
    check(
        &error::Errors(handler.consume()).to_string(),
        expect_test::expect![[r#"
            compiler internal error: custom type present in final intent expr_types slotmap
            compiler internal error: error expression present in final intent exprs slotmap
            compiler internal error: custom type present in final intent expr_types slotmap
            compiler internal error: error expression present in final intent exprs slotmap
            compiler internal error: final intent contains more than one `solve` directive
            compiler internal error: custom type present in final intent expr_types slotmap
            compiler internal error: error expression present in final intent exprs slotmap
            compiler internal error: custom type present in final intent expr_types slotmap
            compiler internal error: error expression present in final intent exprs slotmap"#]],
    );
}
