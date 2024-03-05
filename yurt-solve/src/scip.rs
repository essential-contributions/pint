mod constraint;
mod invert;
mod print;
#[cfg(test)]
mod tests;
mod variable;

use crate::{
    error::SolveError,
    flatyurt::{Decl, FlatYurt, Solve},
};
use russcip::{prelude::*, ProblemCreated, Solved};

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
