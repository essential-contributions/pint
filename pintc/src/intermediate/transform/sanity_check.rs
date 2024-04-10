use slotmap::{SecondaryMap, SlotMap};

use crate::{
    error::{CompileError, Error, ErrorEmitted},
    expr::{Expr, GeneratorKind},
    intermediate::{ExprKey, Handler, IntermediateIntent, Program, SolveFunc},
    span::{empty_span, Span, Spanned},
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

fn check_directive(ii: &IntermediateIntent, handler: &Handler) {
    ii.directives
        .iter()
        .for_each(|(solve_func, span)| match solve_func {
            SolveFunc::Minimize(expr_key) | SolveFunc::Maximize(expr_key) => {
                check_expr(expr_key, span, handler, ii);
            }
            _ => {}
        })
}

fn check_constraints(ii: &IntermediateIntent, handler: &Handler) {
    ii.constraints.iter().for_each(|(expr_key, span)| {
        check_expr(expr_key, span, handler, ii);
    })
}

/* get the constraints array in the ii
recursive move through each exprkey in each constraint
use the exprkey to get the expr and the expr_type
the expr_type must exist or it's an error
the expr_type must not be an illegal type -- see check_expr_types below
then check the expr variant and make sure it's not illegal
if the expr variant contains more exprkeys, recurse
all expressions should resolve to immediates or paths or fn calls
if not, recurse
The goal is to make sure all constraints are sane
 */

fn check_expr(expr_key: &ExprKey, span: &Span, handler: &Handler, ii: &IntermediateIntent) {
    // the expr_type must exist or it's an error
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

    // the expr_type must not be an illegal type -- see check_expr_types below
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

    // the expr must exist or it's an error
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

    println!("checking constraint: {:?}", expr);

    // then check the expr variant and make sure it's not illegal
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
        Expr::UnaryOp { expr, span, .. } => {
            check_expr(expr, span, handler, ii);
        }
        Expr::BinaryOp { lhs, rhs, span, .. } => {
            check_expr(lhs, span, handler, ii);
            check_expr(rhs, span, handler, ii);
        }
        _ => {}
    }
}

fn check_expr_types(ii: &IntermediateIntent, handler: &Handler) {
    ii.expr_types
        .iter()
        .for_each(|(_, expr_type)| match expr_type {
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
        })
}

fn check_exprs(ii: &IntermediateIntent, handler: &Handler) {
    fn check_expr(expr: &Expr, handler: &Handler) {
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
            _ => {}
        }
    }

    if ii.exprs.len() != ii.expr_types.len() {
        handler.emit_err(Error::Compile {
            error: CompileError::Internal {
                msg: "mismatched final intent exprs and expr_types slotmaps",
                span: empty_span(),
            },
        });
    };

    for (expr_key, expr) in ii.exprs.iter() {
        check_expr(expr, handler);

        if ii.expr_types.get(expr_key).is_none() {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "final intent expr_types slotmap is missing corresponding key from exprs slotmap", 
                span: ii.exprs[expr_key].span().clone() }});
        }
    }
}

fn check_vars(ii: &IntermediateIntent, handler: &Handler) {
    // TODO: if we have a var, there must be a corresponding var_type - done
    // same for state - done
    // and exprs - done
    // check the paths work for the solve directive
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

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[cfg(test)]
fn run_test(src: &str) -> String {
    use crate::error;
    error::Errors(run_without_transforms(src)).to_string()
}

#[cfg(test)]
fn run_without_transforms(src: &str) -> Vec<Error> {
    let handler = Handler::default();
    let parsed_source = run_parser(src, &handler);
    let mut type_checked_source = parsed_source
        .type_check(&handler)
        .expect("Failed to type check");
    let _ = sanity_check(&handler, &mut type_checked_source);
    handler.consume()
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
        expect_test::expect![[r#"
        compiler internal error: mismatched final intent exprs and expr_types slotmaps
        compiler internal error: macro call present in final intent exprs slotmap
        compiler internal error: final intent expr_types slotmap is missing corresponding key from exprs slotmap
        compiler internal error: macro call present in final intent exprs slotmap
        compiler internal error: final intent expr_types slotmap is missing corresponding key from exprs slotmap"#]],
    );
    // tuple and tuple field access
    let src = "let t = { y: 3, 2 };
    let x = t.1;";
    check(
        &run_test(src),
        expect_test::expect![[r#"
        compiler internal error: tuple present in final intent expr_types slotmap
        compiler internal error: tuple present in final intent expr_types slotmap
        compiler internal error: tuple present in final intent expr_types slotmap
        compiler internal error: tuple present in final intent exprs slotmap
        compiler internal error: tuple field access present in final intent exprs slotmap
        compiler internal error: tuple present in final intent var_types slotmap"#]],
    );
    // array and array field access
    let src = "let a = [1, 2, 3];
    let b = a[1];";
    check(
        &run_test(src),
        expect_test::expect![[r#"
        compiler internal error: array present in final intent expr_types slotmap
        compiler internal error: array present in final intent expr_types slotmap
        compiler internal error: array present in final intent expr_types slotmap
        compiler internal error: array present in final intent exprs slotmap
        compiler internal error: array element access present in final intent exprs slotmap
        compiler internal error: array present in final intent var_types slotmap"#]],
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
        expect_test::expect![[r#"
        compiler internal error: array present in final intent expr_types slotmap
        compiler internal error: array present in final intent exprs slotmap
        compiler internal error: in expression in final intent exprs slotmap"#]],
    );
    // range
    let src = "let x: int = 3..5;";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: range present in final intent exprs slotmap"#
        ]],
    );
    // forall
    let src = "let k: int;
    constraint forall i in 0..3, j in 0..3 where !(i >= j), i - 1 >= 0 && j > 0 { !(i - j < k) };";
    check(
        &run_test(src),
        expect_test::expect![[r#"
        compiler internal error: range present in final intent exprs slotmap
        compiler internal error: range present in final intent exprs slotmap
        compiler internal error: forall generator present in final intent exprs slotmap"#]],
    );
    // exists
    let src = "let a: int[2][2];
    constraint exists i in 0..1, j in 0..1 {
        a[i][j] == 70
    };";
    check(
        &run_test(src),
        expect_test::expect![[r#"
        compiler internal error: array present in final intent expr_types slotmap
        compiler internal error: array present in final intent expr_types slotmap
        compiler internal error: range present in final intent exprs slotmap
        compiler internal error: range present in final intent exprs slotmap
        compiler internal error: array element access present in final intent exprs slotmap
        compiler internal error: array element access present in final intent exprs slotmap
        compiler internal error: exists generator present in final intent exprs slotmap
        compiler internal error: array present in final intent var_types slotmap"#]],
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
        compiler internal error: array present in final intent exprs slotmap
        compiler internal error: array present in final intent var_types slotmap"#]],
    );
    // tuple
    let src = "let t = { x: 5, 3 };";
    check(
        &run_test(src),
        expect_test::expect![[r#"
        compiler internal error: tuple present in final intent expr_types slotmap
        compiler internal error: tuple present in final intent expr_types slotmap
        compiler internal error: tuple present in final intent exprs slotmap
        compiler internal error: tuple present in final intent var_types slotmap"#]],
    );
    // custom / enum
    let src = "enum MyEnum = Variant1 | Variant2;
    let x = MyEnum;";
    check(
        &run_test(src),
        expect_test::expect![[r#"
        compiler internal error: custom type present in final intent expr_types slotmap
        compiler internal error: custom type present in final intent expr_types slotmap
        compiler internal error: custom type present in final intent var_types slotmap"#]],
    );
    // type alias
    let src = "type MyAliasInt = int;
    let x: MyAliasInt = 3;";
    check(
        &run_test(src),
        expect_test::expect![[r#"
        compiler internal error: type alias present in final intent expr_types slotmap
        compiler internal error: type alias present in final intent var_types slotmap"#]],
    )
}

#[test]
fn var_types() {
    // tuple
    let src = "let t: { int, real, string };";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: tuple present in final intent var_types slotmap"#
        ]],
    );
    // array
    let src = "let a: int[5];";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: array present in final intent var_types slotmap"#
        ]],
    );
    // custom
    let src = "let x: MyEnum;";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: custom type present in final intent var_types slotmap"#
        ]],
    );
    // type alias
    let src = "type MyAliasInt = int;
    let x: MyAliasInt;";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: type alias present in final intent var_types slotmap"#
        ]],
    )
}
