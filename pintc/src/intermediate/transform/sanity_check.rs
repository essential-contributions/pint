use crate::{
    error::{CompileError, Error, ErrorEmitted},
    expr::{Expr, GeneratorKind},
    intermediate::{ExprKey, Handler, IntermediateIntent, Program, SolveFunc},
    span::empty_span,
    types::Type,
};

pub(crate) fn sanity_check(handler: &Handler, program: &mut Program) -> Result<(), ErrorEmitted> {
    program.iis.values().for_each(|ii| {
        check_constraints(ii, handler);
        check_vars(ii, handler);
        check_states(ii, handler);
        check_directive(ii, handler);
    });

    Ok(())
}

fn check_vars(ii: &IntermediateIntent, handler: &Handler) {
    if ii.vars.len() != ii.var_types.len() {
        handler.emit_err(Error::Compile {
            error: CompileError::Internal {
                msg: "mismatched final intent vars and var_types slotmaps",
                span: empty_span(),
            },
        });
    };

    for (var_key, _) in ii.vars.iter() {
        if ii.var_types.get(var_key).is_none() {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                msg:
                    "final intent var_types slotmap is missing corresponding key from vars slotmap",
                span: ii.vars[var_key].span.clone(),
            }});
        }
    }
}

fn check_states(ii: &IntermediateIntent, handler: &Handler) {
    if ii.states.len() != ii.state_types.len() {
        handler.emit_err(Error::Compile {
            error: CompileError::Internal {
                msg: "mismatched final intent states and state_types slotmaps",
                span: empty_span(),
            },
        });
    };

    for (state_key, _) in ii.states.iter() {
        if ii.state_types.get(state_key).is_none() {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                msg:
                    "final intent state_types slotmap is missing corresponding key from states slotmap",
                span: ii.states[state_key].span.clone(),
            }});
        }
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
                check_expr(expr_key, handler, ii);
            }
            _ => {}
        })
}

fn check_constraints(ii: &IntermediateIntent, handler: &Handler) {
    ii.constraints.iter().for_each(|(expr_key, _)| {
        check_expr(expr_key, handler, ii);
    })
}

fn check_expr(expr_key: &ExprKey, handler: &Handler, ii: &IntermediateIntent) {
    let expr_type = match ii.expr_types.get(*expr_key) {
        Some(expr_type) => expr_type,
        None => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "invalid intermediate intent expr_types slotmap key",
                    span: empty_span(),
                },
            });
            return;
        }
    };

    // validate the expr_type is legal
    match expr_type {
        Type::Error(span) => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "error expression present in final intent expr_types slotmap",
                    span: span.clone(),
                },
            });
        }
        Type::Array { span, .. } => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "array present in final intent expr_types slotmap",
                    span: span.clone(),
                },
            });
        }
        Type::Tuple { span, .. } => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "tuple present in final intent expr_types slotmap",
                    span: span.clone(),
                },
            });
        }
        Type::Custom { span, .. } => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "custom type present in final intent expr_types slotmap",
                    span: span.clone(),
                },
            });
        }
        Type::Alias { span, .. } => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "type alias present in final intent expr_types slotmap",
                    span: span.clone(),
                },
            });
        }
        _ => {}
    }

    let expr = match ii.exprs.get(*expr_key) {
        Some(expr) => expr,
        None => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "invalid intermediate intent exprs slotmap key",
                    span: empty_span(),
                },
            });
            return;
        }
    };

    // then check the expr variant and make sure legal
    match expr {
        Expr::Error(span) => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "error expression present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        }
        Expr::MacroCall { span, .. } => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "macro call present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        }
        Expr::If { span, .. } => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "if expression present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        }
        Expr::Array { span, .. } => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "array present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        }
        Expr::ArrayElementAccess { span, .. } => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "array element access present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        }
        Expr::Tuple { span, .. } => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "tuple present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        }
        Expr::TupleFieldAccess { span, .. } => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "tuple field access present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        }
        Expr::Cast { span, .. } => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "cast present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        }
        Expr::In { span, .. } => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "in expression in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        }
        Expr::Range { span, .. } => {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "range present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        }
        Expr::Generator { kind, span, .. } => match kind {
            GeneratorKind::ForAll => {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "forall generator present in final intent exprs slotmap",
                        span: span.clone(),
                    },
                });
            }
            GeneratorKind::Exists => {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "exists generator present in final intent exprs slotmap",
                        span: span.clone(),
                    },
                });
            }
        },
        Expr::UnaryOp { expr, .. } => {
            check_expr(expr, handler, ii);
        }
        Expr::BinaryOp { lhs, rhs, .. } => {
            check_expr(lhs, handler, ii);
            check_expr(rhs, handler, ii);
        }
        _ => {}
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
    let _ = sanity_check(&handler, &mut program);
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
fn exprs() {
    // macrocall
    let src = "macro @equal($x, $y) {
        $x == $y
    }
    macro @foo($x) {
        let a: int;
        constraint a == $x;
    }
    intent Foo {
       @foo(3);
       constraint @equal(4; 4);
    }";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: invalid intermediate intent expr_types slotmap key"#
        ]],
    );
    // tuple and tuple field access
    let src = "let t = { y: 3, 2 };
    let x = t.1;";
    check(
        &run_test(src),
        expect_test::expect![[r#"
        compiler internal error: tuple present in final intent expr_types slotmap
        compiler internal error: tuple present in final intent expr_types slotmap
        compiler internal error: tuple present in final intent exprs slotmap
        compiler internal error: tuple field access present in final intent exprs slotmap"#]],
    );
    // array and array field access
    let src = "let a = [1, 2, 3];
    let b = a[1];";
    check(
        &run_test(src),
        expect_test::expect![[r#"
        compiler internal error: array present in final intent expr_types slotmap
        compiler internal error: array present in final intent expr_types slotmap
        compiler internal error: array present in final intent exprs slotmap
        compiler internal error: array element access present in final intent exprs slotmap"#]],
    );
    // if
    let src = "let b: int;
    let c = false;
    constraint b < if c { 22 } else { 33 };";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: if expression present in final intent exprs slotmap"#
        ]],
    );
    // cast
    let src = "let x: real = 5 as real;";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: cast present in final intent exprs slotmap"#
        ]],
    );
    // in
    let src = "let x: bool = 5 in [3, 4, 5];";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: in expression in final intent exprs slotmap"#
        ]],
    );
    // forall
    let src = "let k: int;
    constraint forall i in 0..3, j in 0..3 where !(i >= j), i - 1 >= 0 && j > 0 { !(i - j < k) };";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: forall generator present in final intent exprs slotmap"#
        ]],
    );
    // exists
    let src = "let a: int[2][2];
    constraint exists i in 0..1, j in 0..1 {
        a[i][j] == 70
    };";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: exists generator present in final intent exprs slotmap"#
        ]],
    );
}

#[test]
fn expr_types() {
    // array
    let src = "let a = [1, 2, 3];";
    check(
        &run_test(src),
        expect_test::expect![[r#"
        compiler internal error: array present in final intent expr_types slotmap
        compiler internal error: array present in final intent expr_types slotmap
        compiler internal error: array present in final intent exprs slotmap"#]],
    );
    // tuple
    let src = "let t = { x: 5, 3 };";
    check(
        &run_test(src),
        expect_test::expect![[r#"
        compiler internal error: tuple present in final intent expr_types slotmap
        compiler internal error: tuple present in final intent expr_types slotmap
        compiler internal error: tuple present in final intent exprs slotmap"#]],
    );
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
fn states() {
    use crate::error;
    use crate::intermediate::State;

    let src = "let a = 1;";
    let (mut program, handler) = run_without_transforms(src);
    program.iis.iter_mut().for_each(|(_, ii)| {
        let dummy_expr_key = ii.exprs.insert(Expr::Error(empty_span()));
        let dummy_state = State {
            name: "test".to_owned(),
            expr: dummy_expr_key,
            span: empty_span(),
        };
        ii.states.insert(dummy_state);
    });
    let _ = sanity_check(&handler, &mut program);
    check(
        &error::Errors(handler.consume()).to_string(),
        expect_test::expect![[r#"
        compiler internal error: mismatched final intent states and state_types slotmaps
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
        ii.vars.insert(Var {
            name: "test".to_owned(),
            span: empty_span(),
        });
    });
    let _ = sanity_check(&handler, &mut program);
    check(
        &error::Errors(handler.consume()).to_string(),
        expect_test::expect![[r#"
        compiler internal error: mismatched final intent vars and var_types slotmaps
        compiler internal error: final intent var_types slotmap is missing corresponding key from vars slotmap"#]],
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

        let dummy_expr_key = ii.exprs.insert(Expr::Error(empty_span()));
        ii.expr_types.insert(
            dummy_expr_key,
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
    let _ = sanity_check(&handler, &mut program);
    check(
        &error::Errors(handler.consume()).to_string(),
        expect_test::expect![[r#"
        compiler internal error: final intent contains more than one `solve` directive
        compiler internal error: custom type present in final intent expr_types slotmap
        compiler internal error: error expression present in final intent exprs slotmap
        compiler internal error: custom type present in final intent expr_types slotmap
        compiler internal error: error expression present in final intent exprs slotmap"#]],
    );
}
