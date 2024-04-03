use crate::{
    error::{CompileError, Error, ErrorEmitted},
    expr::Expr,
    intermediate::{Handler, IntermediateIntent, Program},
    span::{empty_span, Spanned},
    types::Type,
};

pub(crate) fn sanity_check(handler: &Handler, program: &mut Program) -> Result<(), ErrorEmitted> {
    program.iis.values().for_each(|ii| {
        check_expr_types(ii, handler);
        check_exprs(ii, handler);
        check_var_types(ii, handler);
        check_vars(ii, handler);
    });

    Ok(())
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
            Expr::ForAll { span, .. } => {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "forall present in final intent exprs slotmap",
                        span: span.clone(),
                    },
                });
            }
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
fn check_var_types(ii: &IntermediateIntent, handler: &Handler) {
    ii.var_types
        .iter()
        .for_each(|(_, var_type)| match var_type {
            Type::Error(span) => {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "error var present in final intent var_types slotmap",
                        span: span.clone(),
                    },
                });
            }
            Type::Array { span, .. } => {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "array present in final intent var_types slotmap",
                        span: span.clone(),
                    },
                });
            }
            Type::Tuple { span, .. } => {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "tuple present in final intent var_types slotmap",
                        span: span.clone(),
                    },
                });
            }
            Type::Custom { span, .. } => {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "custom type present in final intent var_types slotmap",
                        span: span.clone(),
                    },
                });
            }
            Type::Alias { span, .. } => {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "type alias present in final intent var_types slotmap",
                        span: span.clone(),
                    },
                });
            }
            _ => {}
        });
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
    use crate::parser::yurt_parser;
    use crate::span;
    use std::collections::BTreeMap;

    let parser = yurt_parser::YurtParser::new();
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
        compiler internal error: forall present in final intent exprs slotmap"#]],
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

// TODO: Identify which tests are causing which issues. Open an issue for each type.
// each issue is probably an easy fix and there is only a few areas that are causing the issue
// explain exactly what's going on in each issue. "My sanity check shows x. We actually want x."
// Can easily reach out to Toby and Mohammad to help with tough ones
// just triage the problems first
