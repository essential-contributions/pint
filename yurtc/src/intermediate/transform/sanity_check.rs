use crate::{
    error::CompileError,
    expr::Expr,
    intermediate::{IntermediateIntent, Program},
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

pub(crate) fn sanity_check(program: &mut Program) -> Result<(), Vec<CompileError>> {
    let errors: Vec<CompileError> = program
        .iis
        .values()
        .flat_map(|ii| {
            println!("{:#?}", ii);
            let mut ii_errors = check_expr_types(ii);
            println!("{:?}", ii_errors);
            check_exprs(ii, &mut ii_errors);
            check_var_types(ii, &mut ii_errors);
            check_vars(ii, &mut ii_errors);
            ii_errors
        })
        .collect();

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn check_expr_types(ii: &IntermediateIntent) -> Vec<CompileError> {
    ii.expr_types
        .iter()
        .filter_map(|(_, expr_type)| match expr_type {
            Type::Error(span) => Some(CompileError::Internal {
                msg: "error expression present in final intent expr_types slotmap",
                span: span.clone(),
            }),
            Type::Array { span, .. } => Some(CompileError::Internal {
                msg: "array present in final intent expr_types slotmap",
                span: span.clone(),
            }),
            Type::Tuple { span, .. } => Some(CompileError::Internal {
                msg: "tuple present in final intent expr_types slotmap",
                span: span.clone(),
            }),
            Type::Custom { span, .. } => Some(CompileError::Internal {
                msg: "custom type present in final intent expr_types slotmap",
                span: span.clone(),
            }),
            Type::Alias { span, .. } => Some(CompileError::Internal {
                msg: "type alias present in final intent expr_types slotmap",
                span: span.clone(),
            }),
            _ => None,
        })
        .collect::<Vec<CompileError>>()
}

fn check_exprs(ii: &IntermediateIntent, errors: &mut Vec<CompileError>) {
    fn check_expr(expr: &Expr) -> Option<CompileError> {
        match expr {
            Expr::Error(span) => Some(CompileError::Internal {
                msg: "error expression present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::MacroCall { span, .. } => Some(CompileError::Internal {
                msg: "macro call present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::If { span, .. } => Some(CompileError::Internal {
                msg: "if expression present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::Array { span, .. } => Some(CompileError::Internal {
                msg: "array present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::ArrayElementAccess { span, .. } => Some(CompileError::Internal {
                msg: "array element access present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::Tuple { span, .. } => Some(CompileError::Internal {
                msg: "tuple present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::TupleFieldAccess { span, .. } => Some(CompileError::Internal {
                msg: "tuple field access present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::Cast { span, .. } => Some(CompileError::Internal {
                msg: "cast present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::In { span, .. } => Some(CompileError::Internal {
                msg: "in expression in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::Range { span, .. } => Some(CompileError::Internal {
                msg: "range present in final intent exprs slotmap",
                span: span.clone(),
            }),
            Expr::ForAll { span, .. } => Some(CompileError::Internal {
                msg: "forall present in final intent exprs slotmap",
                span: span.clone(),
            }),
            _ => None,
        }
    }

    if ii.exprs.len() != ii.expr_types.len() {
        errors.push(CompileError::Internal {
            msg: "mismatched final intent exprs and expr_types slotmaps",
            span: empty_span(),
        });
    };

    for (expr_key, expr) in ii.exprs.iter() {
        if let Some(error) = check_expr(expr) {
            errors.push(error);
        }

        if ii.expr_types.get(expr_key).is_none() {
            errors.push(CompileError::Internal {
                msg: "final intent expr_types slotmap is missing corresponding key from exprs slotmap", 
                span: ii.exprs[expr_key].span().clone() })
        }
    }
}

fn check_vars(ii: &IntermediateIntent, errors: &mut Vec<CompileError>) {
    if ii.vars.len() != ii.var_types.len() {
        errors.push(CompileError::Internal {
            msg: "mismatched final intent vars and var_types slotmaps",
            span: empty_span(),
        });
    };

    let mut errors: Vec<CompileError> = Vec::new();
    for (var_key, _) in ii.vars.iter() {
        if ii.var_types.get(var_key).is_none() {
            errors.push(CompileError::Internal {
                msg:
                    "final intent var_types slotmap is missing corresponding key from vars slotmap",
                span: ii.vars[var_key].span.clone(),
            })
        }
    }
}

fn check_var_types(ii: &IntermediateIntent, errors: &mut Vec<CompileError>) {
    errors.append(
        &mut ii
            .var_types
            .iter()
            .filter_map(|(_, var_type)| match var_type {
                Type::Error(span) => Some(CompileError::Internal {
                    msg: "error var present in final intent var_types slotmap",
                    span: span.clone(),
                }),
                Type::Array { span, .. } => Some(CompileError::Internal {
                    msg: "array present in final intent var_types slotmap",
                    span: span.clone(),
                }),
                Type::Tuple { span, .. } => Some(CompileError::Internal {
                    msg: "tuple present in final intent var_types slotmap",
                    span: span.clone(),
                }),
                Type::Custom { span, .. } => Some(CompileError::Internal {
                    msg: "custom type present in final intent var_types slotmap",
                    span: span.clone(),
                }),
                Type::Alias { span, .. } => Some(CompileError::Internal {
                    msg: "type alias present in final intent var_types slotmap",
                    span: span.clone(),
                }),
                _ => None,
            })
            .collect::<Vec<CompileError>>(),
    );
}

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[cfg(test)]
fn run_test(src: &str) -> String {
    format!("{}", run_without_transforms(src))
}

#[cfg(test)]
fn run_without_transforms(src: &str) -> crate::error::Errors {
    use crate::error::Error;

    let parsed_source = run_parser(src);
    let mut type_checked_source = parsed_source.type_check().expect("Failed to type check");
    match sanity_check(&mut type_checked_source) {
        Ok(_) => crate::error::Errors(vec![]),
        Err(errors) => {
            let mut final_errors = vec![];
            for error in errors {
                final_errors.push(Error::Compile { error });
            }
            crate::error::Errors(final_errors)
        }
    }
}

#[cfg(test)]
fn run_parser(src: &str) -> Program {
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
            &mut Vec::new(),
            crate::lexer::Lexer::new(src, &filepath, &[]),
        )
        .expect("Failed to parse test case.");

    program
}

#[test]
fn exprs() {
    let src = "let a = [1, 2, 3];";
    check(
        &run_test(src),
        expect_test::expect![[
            r#"compiler internal error: array present in final intent expr_types slotmap
            compiler internal error: array present in final intent expr_types slotmap
            compiler internal error: array present in final intent exprs slotmap
            compiler internal error: array present in final intent var_types slotmap"#
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
