mod constraint;
mod invert;
mod print;
#[cfg(test)]
mod tests;
mod variable;

use crate::{
    error::SolveError,
    flatyurt::{evaluate::evaluate_expr, Decl, FlatYurt, Immediate, Solve},
};
use russcip::{prelude::*, ProblemCreated, Solved};
use std::collections::HashMap;

pub struct Solver<'a, State> {
    model: Model<State>,
    flatyurt: &'a FlatYurt,
    unique_var_suffix: usize, // unique suffix for variables introduced by the solver
    unique_cons_suffix: usize, // unique suffix for names of cosntraints
}

impl<'a, State> Solver<'a, State> {
    /// Creates a new instance of `Solver` given a `FlatYurt` instance.
    pub fn new(flatyurt: &'a FlatYurt) -> Solver<ProblemCreated> {
        Solver {
            model: Model::new()
                .hide_output()
                .include_default_plugins()
                .create_prob("solver")
                .set_obj_sense(match flatyurt.solve {
                    // For constraint satisfaction problems, `ObjSense` does not matter. The
                    // objective function is going to be set to 0 anyways.
                    Solve::Minimize(_) | Solve::Satisfy => ObjSense::Minimize,
                    Solve::Maximize(_) => ObjSense::Maximize,
                }),
            flatyurt,
            unique_var_suffix: 0,
            unique_cons_suffix: 0,
        }
    }

    fn new_var_name(&mut self) -> String {
        let new_name = format!("INTRODUCED{}", self.unique_var_suffix);
        self.unique_var_suffix += 1;
        new_name
    }

    fn new_cons_name(&mut self) -> String {
        let new_name = format!("CONS{}", self.unique_cons_suffix);
        self.unique_cons_suffix += 1;
        new_name
    }
}

impl<'a> Solver<'a, ProblemCreated> {
    pub fn solve(mut self) -> Result<Solver<'a, Solved>, SolveError> {
        // Convert all variables first
        for decl in &self.flatyurt.decls {
            if let Decl::Var(var) = decl {
                self.convert_variable(var);
            }
        }

        // Now convert all the constraints
        for decl in &self.flatyurt.decls {
            if let Decl::Constraint(constraint) = decl {
                self.convert_constraint(&constraint.0)?;
            }
        }

        Ok(Solver {
            model: self.model.solve(),
            flatyurt: self.flatyurt,
            unique_var_suffix: self.unique_var_suffix,
            unique_cons_suffix: self.unique_cons_suffix,
        })
    }
}

impl<'a> super::Solver<'a, Solved> {
    /// Returns a `HashMap` that contains a solution. It maps decision variable names, as
    /// `String`s, to `flatyurt::Immediate` values. Returns an empty `HashMap` in case a solution
    /// cannot be found
    #[allow(unused)]
    pub fn solution(&self) -> HashMap<String, Immediate> {
        match self.model.status() {
            Status::Optimal => {
                let sol = self.model.best_sol().unwrap();

                // Assume that the vars in `self.flatyurt` have been converted first before any
                // additional helper variables are introduced. That is, if `self.flatyurt`
                // contains `n` variables, then the first `n` elements of `self.model.vars()`
                // correspond to those `n` variables.
                self.model
                    .vars()
                    .iter()
                    .take(
                        self.flatyurt
                            .decls
                            .iter()
                            .filter(|decl| matches!(decl, Decl::Var(_)))
                            .count(),
                    )
                    .map(|var| {
                        (
                            var.name(),
                            match var.var_type() {
                                VarType::Binary => Immediate::Bool(sol.val(var.clone()) != 0.0),
                                VarType::Integer | VarType::ImplInt => {
                                    Immediate::Int(sol.val(var.clone()) as i64)
                                }
                                VarType::Continuous => Immediate::Real(sol.val(var.clone())),
                            },
                        )
                    })
                    .collect::<HashMap<_, _>>()
            }
            _ => HashMap::new(),
        }
    }

    /// Verify that the solution produced by the solver satisfies the constraints. This is mostly
    /// useful for testing purposes.
    pub fn verify_solution(&self) -> bool {
        let solution = self.solution();
        let constraints = self
            .flatyurt
            .decls
            .iter()
            .filter_map(|decl| match decl {
                Decl::Constraint(constraint) => Some(constraint),
                _ => None,
            })
            .collect::<Vec<_>>();

        constraints
            .iter()
            .all(|constraint| match evaluate_expr(&constraint.0, &solution) {
                Immediate::Bool(val) => val,
                Immediate::Int(1) => true,
                Immediate::Int(0) => false,
                _ => panic!("constraint expression cannot evaluate to non-bool"),
            })
    }
}
