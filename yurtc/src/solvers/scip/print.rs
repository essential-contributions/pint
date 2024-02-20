use crate::intermediate::SolveFunc;
use russcip::{prelude::*, Solved};
use std::fmt::Write;
use yansi::{Color, Style};

impl<'a> super::Solver<'a, Solved> {
    /// Pretty print the output of the solver which includes a valid solution for all the variables
    /// in case of success. Skips any helper variables introduced by the solver. Rounds to 3
    /// decimal places.
    pub fn print_solution(&self) {
        match self.model.status() {
            Status::Optimal => {
                println!(
                    "   {}",
                    Style::new(Color::Green)
                        .bold()
                        .paint("Problem is satisfiable")
                );
                println!("    {}", Style::new(Color::Green).bold().paint("Solution:"));
                let sol = self.model.best_sol().unwrap();

                self.model
                    .vars()
                    .iter()
                    .take(self.intent.vars.len())
                    .fold((), |(), var| {
                        if var.var_type() == VarType::Continuous {
                            println!("     {}: {:.3}", &var.name(), sol.val(var.clone()));
                        } else {
                            println!("     {}: {}", &var.name(), sol.val(var.clone()));
                        }
                    });

                // Assume that there exists exactly a single solve directive. This should have been
                // checked by now
                if !matches!(self.intent.directives[0].0, SolveFunc::Satisfy) {
                    println!(
                        "    {}: {:.3}",
                        Style::new(Color::Green).bold().paint("Objective value:"),
                        self.model.obj_val()
                    );
                }
            }
            Status::Infeasible => println!(
                "   {}",
                Style::new(Color::Red).bold().paint("Problem is infeasible")
            ),
            Status::Unbounded => println!(
                "   {}",
                Style::new(Color::Red).bold().paint("Problem is unbounded")
            ),
            Status::Inforunbd => println!(
                "   {}",
                Style::new(Color::Red)
                    .bold()
                    .paint("Problem is infeasible or unbounded")
            ),
            _ => println!(
                "   {} {:?}",
                Style::new(Color::Red).bold().paint("SCIP status:"),
                Style::new(Color::Red).bold().paint(self.model.status())
            ),
        }
    }

    /// Serialize the raw output of the solver into a `String`. This is mostly meant for testing
    /// purposes. Includes helper variables introduced by the solver. Rounds to 3 decimal places.
    pub fn display_solution_raw(&self) -> String {
        match self.model.status() {
            Status::Optimal => {
                let sol = self.model.best_sol().unwrap();
                let solution = self.model.vars().iter().take(self.intent.vars.len()).fold(
                    String::new(),
                    |mut acc, var| {
                        if var.var_type() == VarType::Continuous {
                            writeln!(&mut acc, "{}: {:.3}", &var.name(), sol.val(var.clone()))
                                .expect("Failed to write solution to string");
                        } else {
                            writeln!(&mut acc, "{}: {}", &var.name(), sol.val(var.clone()))
                                .expect("Failed to write solution to string");
                        }
                        acc
                    },
                );

                // Assume that there exists exactly a single solve directive. This should have been
                // checked by now
                if matches!(self.intent.directives[0].0, SolveFunc::Satisfy) {
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
