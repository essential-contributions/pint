use crate::{
    error::{CompileError, Error, ErrorEmitted},
    expr::Expr,
    intermediate::{Handler, IntermediateIntent, Program},
    span::{empty_span, Spanned},
    types::Type,
};

// make sure no foralls
// no arrays
// no type aliases
// check there is a matching expr and expr_type for everything
// check there is a matching var and var_type for everything
// check verification pass has to be the last pass
// ideally all internal compile errors, and not user facing

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
    ii.expr_types.iter().for_each(|(_, expr_type)| {
        if let Type::Error(span) = expr_type {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "error expression present in final intent expr_types slotmap",
                    span: span.clone(),
                },
            });
        } else if let Type::Array { span, .. } = expr_type {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "array present in final intent expr_types slotmap",
                    span: span.clone(),
                },
            });
        } else if let Type::Tuple { span, .. } = expr_type {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "tuple present in final intent expr_types slotmap",
                    span: span.clone(),
                },
            });
        } else if let Type::Custom { span, .. } = expr_type {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "custom type present in final intent expr_types slotmap",
                    span: span.clone(),
                },
            });
        } else if let Type::Alias { span, .. } = expr_type {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "type alias present in final intent expr_types slotmap",
                    span: span.clone(),
                },
            });
        }
    })
}

fn check_exprs(ii: &IntermediateIntent, handler: &Handler) {
    fn check_expr(expr: &Expr, handler: &Handler) {
        if let Expr::Error(span) = expr {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "error expression present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        } else if let Expr::MacroCall { span, .. } = expr {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "macro call present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        } else if let Expr::If { span, .. } = expr {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "if expression present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        } else if let Expr::Array { span, .. } = expr {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "array present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        } else if let Expr::ArrayElementAccess { span, .. } = expr {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "array element access present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        } else if let Expr::Tuple { span, .. } = expr {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "tuple present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        } else if let Expr::TupleFieldAccess { span, .. } = expr {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "tuple field access present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        } else if let Expr::Cast { span, .. } = expr {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "cast present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        } else if let Expr::In { span, .. } = expr {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "in expression in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        } else if let Expr::Range { span, .. } = expr {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "range present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
        } else if let Expr::ForAll { span, .. } = expr {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "forall present in final intent exprs slotmap",
                    span: span.clone(),
                },
            });
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
    ii.var_types.iter().for_each(|(_, var_type)| {
        if let Type::Error(span) = var_type {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "error var present in final intent var_types slotmap",
                    span: span.clone(),
                },
            });
        } else if let Type::Array { span, .. } = var_type {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "array present in final intent var_types slotmap",
                    span: span.clone(),
                },
            });
        } else if let Type::Tuple { span, .. } = var_type {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "tuple present in final intent var_types slotmap",
                    span: span.clone(),
                },
            });
        } else if let Type::Custom { span, .. } = var_type {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "custom type present in final intent var_types slotmap",
                    span: span.clone(),
                },
            });
        } else if let Type::Alias { span, .. } = var_type {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "type alias present in final intent var_types slotmap",
                    span: span.clone(),
                },
            });
        }
    });
}

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[cfg(test)]
fn run_test(src: &str) -> String {
    use crate::error;

    let errors = run_without_transforms(src);
    let errors = error::Errors(errors);
    errors.to_string()
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
fn expr_types() {
    let src = "let a = [1, 2, 3];";
    check(
        &run_test(src),
        expect_test::expect![[r#"
            compiler internal error: array present in final intent expr_types slotmap
            compiler internal error: array present in final intent expr_types slotmap
            compiler internal error: array present in final intent exprs slotmap
            compiler internal error: array present in final intent var_types slotmap"#]],
    );
    let src = "let t = { x: 5, 3 };";
    check(
        &run_test(src),
        expect_test::expect![[r#"
            compiler internal error: tuple present in final intent expr_types slotmap
            compiler internal error: tuple present in final intent expr_types slotmap
            compiler internal error: tuple present in final intent exprs slotmap
            compiler internal error: tuple present in final intent var_types slotmap"#]],
    );
    let src = "enum MyEnum = Variant1 | Variant2;
    let x = MyEnum;";
    check(
        &run_test(src),
        expect_test::expect![[r#"
        compiler internal error: custom type present in final intent expr_types slotmap
        compiler internal error: custom type present in final intent expr_types slotmap
        compiler internal error: custom type present in final intent var_types slotmap"#]],
    );
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
    let src = "let t: { int, real, string };";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: tuple present in final intent var_types slotmap"#
        ]],
    );
    let src = "let a: int[5];";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: array present in final intent var_types slotmap"#
        ]],
    );
    let src = "let x: MyEnum;";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: custom type present in final intent var_types slotmap"#
        ]],
    );
    let src = "type MyAliasInt = int;
    let x: MyAliasInt;";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: type alias present in final intent var_types slotmap"#
        ]],
    )
}

// @mohammad, please check errors in test.yrt. It seems like our final intent is pretty screwed? What is expected from
// let a = [1, 2, 3];
// solve satisfy;

// TODO: add unit tests here
// easiest way is to take a string that contains source code
// run the parser on it
// program will come out of it
// then run the checker directly on that

// ---

// Tests that fail sanity check with transforms applied
/* let a = [1, 2, 3]; */
/* let a : int[3] = [1, 2, 3]; */
/*
type AccountTuple = { id: int, balance: real, address: string };
let walletDetails: AccountTuple = {id: 1, balance: 2.0, address: "0x1234...ABCD"};
 */
/* let t: { x: int, y: real } = { y: 5.0, x: 6 }; */

// Looks like array expr, tuple expr, enum types all fail

// ---

// Tests that don't fail sanity check
/* let a = int[3]; */
/* constraint forall i in 0..17 {
    true
}; */
/* type Address = string;
type AccountTuple = { id: int, balance: real, address: string }; */
/* type Address = string;
let myAddress: Address = "0x1234567890abcdef"; */
/* let x = t.1; */

// Looks like tuple field access, tuple types, new types, foralls, array types, enum decls all work fine
