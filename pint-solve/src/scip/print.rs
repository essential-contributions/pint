use crate::flatpint::{Decl, Solve};
use russcip::{prelude::*, Solved};
use std::fmt::Write;
use yansi::Paint;

impl super::Solver<'_, Solved> {
    /// Pretty print the output of the solver which includes a valid solution for all the variables
    /// in case of success. Skips any helper variables introduced by the solver. Rounds to 3
    /// decimal places.
    pub fn print_solution(&self) {
        match self.model.status() {
            Status::Optimal => {
                println!("   {}", "Problem is satisfiable".bold().green());
                println!("    {}", "Solution:".bold().green());
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
                    .fold((), |(), var| {
                        if var.var_type() == VarType::Continuous {
                            println!("     {}: {:.3}", &var.name(), sol.val(var.clone()));
                        } else {
                            println!("     {}: {}", &var.name(), sol.val(var.clone()));
                        }
                    });

                // Assume that there exists exactly a single solve directive. This should have been
                // checked by now
                if !matches!(self.flatpint.solve, Solve::Satisfy) {
                    println!(
                        "    {}: {:.3}",
                        "Objective value:".bold().green(),
                        self.model.obj_val()
                    );
                }
            }
            Status::Infeasible => println!("   {}", "Problem is infeasible".bold().red()),
            Status::Unbounded => println!("   {}", "Problem is unbounded".bold().red()),
            Status::Inforunbd => {
                println!("   {}", "Problem is infeasible or unbounded".bold().red())
            }
            _ => println!(
                "   {} {:?}",
                "SCIP status:".bold().red(),
                self.model.status().bold().red()
            ),
        }
    }

    /// Serialize the raw output of the solver into a `String`. This is mostly meant for testing
    /// purposes. Includes helper variables introduced by the solver. Rounds to 3 decimal places.
    #[allow(unused)]
    pub fn display_solution_raw(&self) -> String {
        match self.model.status() {
            Status::Optimal => {
                let sol = self.model.best_sol().unwrap();

                // Assume that the vars in `self.flatpint` have been converted first before any
                // additional helper variables are introduced. That is, if `self.flatpint`
                // contains `n` variables, then the first `n` elements of `self.model.vars()`
                // correspond to those `n` variables.
                let solution = self
                    .model
                    .vars()
                    .iter()
                    .take(
                        self.flatpint
                            .decls
                            .iter()
                            .filter(|decl| matches!(decl, Decl::Var(_)))
                            .count(),
                    )
                    .fold(String::new(), |mut acc, var| {
                        if var.var_type() == VarType::Continuous {
                            writeln!(&mut acc, "{}: {:.3}", &var.name(), sol.val(var.clone()))
                                .expect("Failed to write solution to string");
                        } else {
                            writeln!(&mut acc, "{}: {}", &var.name(), sol.val(var.clone()))
                                .expect("Failed to write solution to string");
                        }
                        acc
                    });

                // Assume that there exists exactly a single solve directive. This should have been
                // checked by now
                if matches!(self.flatpint.solve, Solve::Satisfy) {
                    solution
                } else {
                    format!("{solution}objective: {:.3}", self.model.obj_val())
                }
            }
            Status::Infeasible => "infeasible".to_string(),
            Status::Unbounded => "unbounded".to_string(),
            Status::Inforunbd => "infeasible or unbounded".to_string(),
            _ => format!("scip_status({:?})", self.model.status()),
        }
    }
}
