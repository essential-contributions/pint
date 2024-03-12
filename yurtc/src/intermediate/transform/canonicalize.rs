use crate::{
    error::CompileError,
    expr::{Expr, Ident},
    intermediate::{
        IntermediateIntent,
        SolveFunc::{self, *},
    },
};

pub(crate) fn canonicalize(ii: &mut IntermediateIntent) -> Result<(), CompileError> {
    canonicalize_directives(ii)?;

    Ok(())
}

// Exprs and expr_types slot maps share keys
// use the same key for the expr and the type for the same expr
// the key in the solve directive is the corresponding key for the expr in both slot maps

// 1. create a variable
// in the vars slot map add a new var
// the name is tricky, you cannot choose a name that the user could choose in their program
// the way we address this is by choosing a name that is not allowed -- ala how array names are given brackets
// we need something different for directive variables
// one approach in the past was to put __ in front of the variable, we would need to make sure that the user cannot enter that <- new issue
// lets start with __objective -- fine to hard code because we'll only ever have one
// when creating a var you set the name and span in the vars slotmap
// the span should just be the span of the expr that is already in the directive in the ii
// 2. get a key back from the slotmap and use that to insert into the var_types slotmap
// get the type from the expr type in the directive already in the ii
// 3. create a binary expression for the constraint
// binary expr where the left is the path to the var and the right side is the expr key from the directive
// create an expr object and insert in the exprs slotmap
// first you need to create an expr then you can use that expr key in the binary op expr
// make sure each expr get a corresponding slot in the expr_types slotmap
// use the key from the binary expr to insert a type in expr_types and into the constraint vector
// == returns bool so that means the new binaryop expr should be of bool type
// 4. replace the expr key for the solve directive with the expr key for the name expr

// all spans should point to the initial span
// all things that were created are basically inside the original expr
// it means all errors point to that expr

// This is a simple transformation as follows:
// solve maximize <expr>;
// becomes:
// let __objective: <type_of_expr>;
// constraint __objective == <expr>;
// solve maximize __objective;
fn canonicalize_directives(ii: &mut IntermediateIntent) -> Result<(), CompileError> {
    // dbg!(ii);

    // get the directive
    let directive = ii
        .directives
        .first()
        .expect("TODO: handle error if missing directive or more than one directive")
        .clone();
    // @mohammad, do care want an error if there is no directive?
    // -- answered - emit a proper error if missing

    let (solve_func, span) = &directive;
    dbg!(solve_func);
    // @mohammad do we avoid this transform entirely if its not maximize or minimize?
    // -- answered - just return
    let expr_key = match solve_func {
        Satisfy => return Ok(()),
        Minimize(expr_key) | Maximize(expr_key) => expr_key,
    };

    let expr = ii
        .exprs
        .get(*expr_key)
        .expect("todo: handle missing expr key with internal compiler error");

    dbg!(expr);

    let expr_type = ii
        .expr_types
        .get(*expr_key)
        .expect("todo: handle missing expr key with internal compiler error")
        .clone();

    // dbg!(expr_type);

    // create a variable named __objective with the same span of the directive
    // creates var and var_type in appropriate slotmaps
    // @mohammad - we don't need a mod path because there is only one directive and it has to be top level, right?
    // -- answered - no mod path needed and no local scope
    let ident = Ident {
        name: "__objective".to_string(),
        span: span.clone(),
    };
    let expr_type_clone = expr_type.clone();
    let var_key = ii
        .insert_var("", None, &ident, Some(expr_type.clone()))
        .expect("todo: handle parse error as compile error");

    dbg!(&ii.vars);
    dbg!(&ii.var_types);

    let new_expr_key = ii.exprs.insert(Expr::PathByKey(var_key, span.clone()));
    let _ = ii.expr_types.insert(new_expr_key, expr_type_clone);

    // create a binary expression for the constraint
    dbg!(&ii.exprs);
    ii.insert_eq_or_ineq_constraint(var_key, *expr_key, span.clone());

    dbg!(&ii.exprs);

    dbg!(var_key);
    dbg!(*expr_key);
    dbg!(new_expr_key);

    let error_test = &ii.expr_types[*expr_key];
    dbg!(error_test);

    // @mohammad should this be the varkey? Only the name is related to the var, not the expr. If so how could we put that in the solve func?
    // jk, need to make an expression above
    let updated_solve_func = match solve_func {
        Satisfy => todo!(),
        Minimize(_) => SolveFunc::Minimize(new_expr_key),
        Maximize(_) => SolveFunc::Maximize(new_expr_key),
    };
    let updated_directive = (updated_solve_func, span.clone());
    ii.directives[0] = updated_directive;

    println!("{ii}");

    Ok(())
}
