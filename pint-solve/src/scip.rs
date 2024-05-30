mod constraint;
mod invert;
mod print;
#[cfg(test)]
mod tests;
mod variable;

use crate::{
    error::SolveError,
    flatpint::{evaluate::evaluate_expr, Decl, FlatPint, Immediate, Solve},
};
use fxhash::FxHashMap;
use russcip::{prelude::*, ProblemCreated, Solved};

pub struct Solver<'a, State> {
    model: Model<State>,
    flatpint: &'a FlatPint,
    unique_var_suffix: usize, // unique suffix for variables introduced by the solver
    unique_cons_suffix: usize, // unique suffix for names of cosntraints
}

impl<'a, State> Solver<'a, State> {
    /// Creates a new instance of `Solver` given a `FlatPint` instance.
    pub fn new(flatpint: &'a FlatPint) -> Solver<ProblemCreated> {
        Solver {
            model: Model::new()
                .hide_output()
                .include_default_plugins()
                .create_prob("solver")
                .set_obj_sense(match flatpint.solve {
                    // For constraint satisfaction problems, `ObjSense` does not matter. The
                    // objective function is going to be set to 0 anyways.
                    Solve::Minimize(_) | Solve::Satisfy => ObjSense::Minimize,
                    Solve::Maximize(_) => ObjSense::Maximize,
                }),
            flatpint,
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
        for decl in &self.flatpint.decls {
            if let Decl::Var(var) = decl {
                self.convert_variable(var);
            }
        }

        // Now convert all the constraints
        for decl in &self.flatpint.decls {
            if let Decl::Constraint(constraint) = decl {
                self.convert_constraint(&constraint.0)?;
            }
        }

        Ok(Solver {
            model: self.model.solve(),
            flatpint: self.flatpint,
            unique_var_suffix: self.unique_var_suffix,
            unique_cons_suffix: self.unique_cons_suffix,
        })
    }
}

impl<'a> super::Solver<'a, Solved> {
    /// Returns a `FxHashMap` that contains a solution. It maps decision variable names, as
    /// `String`s, to `flatpint::Immediate` values. Returns an empty `FxHashMap` in case a solution
    /// cannot be found
    #[allow(unused)]
    pub fn solution(&self) -> FxHashMap<String, Immediate> {
        match self.model.status() {
            Status::Optimal => {
                let sol = self.model.best_sol().unwrap();

                // Assume that the vars in `self.flatpint` have been converted first before any
                // additional helper variables are introduced. That is, if `self.flatpint`
                // contains `n` variables, then the first `n` elements of `self.model.vars()`
                // correspond to those `n` variables.
                self.model
                    .vars()
                    .iter()
                    .take(
                        self.flatpint
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
                    .collect::<FxHashMap<_, _>>()
            }
            _ => FxHashMap::default(),
        }
    }

    /// Verify that the solution produced by the solver satisfies the constraints. This is mostly
    /// useful for testing purposes.
    pub fn verify_solution(&self) -> bool {
        let solution = self.solution();
        let constraints = self
            .flatpint
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
