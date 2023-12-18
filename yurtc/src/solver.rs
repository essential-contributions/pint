use crate::{
    error::SolveError,
    expr,
    intent::{self, Expression, Intent},
};
use gcollections::ops::*;
use interval::{interval_set::*, ops::Range};
use pcp::{
    concept::*,
    kernel::*,
    propagators::*,
    search::{search_tree_visitor::Status::*, *},
    term::*,
    variable::ops::*,
};
use std::fmt::Write;
use yansi::{Color, Style};

struct SolverVar {
    name: String,
    var: Var<VStore>,
}

pub struct Solver<'a> {
    space: FDSpace,
    variables: Vec<SolverVar>,
    intent: &'a Intent,
    status: Option<Status<FDSpace>>,
}

impl<'a> Solver<'a> {
    pub fn new(intent: &'a Intent) -> Solver {
        Solver {
            space: FDSpace::empty(),
            variables: Vec::new(),
            intent,
            status: None,
        }
    }

    fn expr_to_vstore(&mut self, expr: &Expression) -> Result<Var<VStore>, SolveError> {
        match expr {
            Expression::Path(path) => self
                .variables
                .iter()
                .find(|v| &v.name == path)
                .map(|SolverVar { var, .. }| var.bclone())
                .ok_or(SolveError::Internal {
                    msg: "pcp solver: cannot find variable for path expression",
                }),
            Expression::Immediate(expr::Immediate::Int(val)) => {
                Ok(Box::new(Constant::new(*val as i32)))
            }
            Expression::BinaryOp { op, lhs, rhs } => {
                let lhs = self.expr_to_vstore(lhs)?;
                let rhs = self.expr_to_vstore(rhs)?;
                match op {
                    expr::BinaryOp::Add => Ok(Box::new(Sum::new(vec![lhs, rhs]))),
                    _ => Err(SolveError::Internal {
                        msg: "pcp solver: only addition is expected in expressions",
                    }),
                }
            }
            _ => Err(SolveError::Internal {
                msg:
                    "pcp solver: only paths, immediates, and binary ops are expected in expressions",
            }),
        }
    }

    pub fn solve(mut self) -> Result<Self, SolveError> {
        if !matches!(self.intent.directive, intent::Solve::Satisfy) {
            return Err(SolveError::Internal {
                msg: "pcp solver: only constraint satisfaction problems are currently supported",
            });
        }

        for v in &self.intent.vars {
            // TODO: The bounds chosen below are a temporary hack. One option is to use the largest
            // possible interval: (i32::MIN, i32::MAX), but that causes some arithmetic overflow
            // issues for some problems. Alternatively, an analysis of the program can lead to
            // better bounds.
            self.variables.push(SolverVar {
                name: v.name.clone(),
                var: Box::new(
                    self.space
                        .vstore
                        .alloc(IntervalSet::new(-1_000_000_000, 1_000_000_000)),
                ) as Var<VStore>,
            });
        }

        for constraint in &self.intent.constraints {
            match constraint {
                Expression::BinaryOp { op, lhs, rhs } => {
                    let lhs = self.expr_to_vstore(lhs)?;
                    let rhs = self.expr_to_vstore(rhs)?;
                    match op {
                        expr::BinaryOp::Equal => {
                            self.space.cstore.alloc(Box::new(XEqY::new(lhs, rhs)))
                        }
                        expr::BinaryOp::NotEqual => {
                            self.space.cstore.alloc(Box::new(XNeqY::new(lhs, rhs)))
                        }
                        expr::BinaryOp::GreaterThanOrEqual => {
                            self.space.cstore.alloc(Box::new(x_geq_y(lhs, rhs)))
                        }
                        expr::BinaryOp::GreaterThan => {
                            self.space.cstore.alloc(Box::new(x_greater_y(lhs, rhs)))
                        }
                        expr::BinaryOp::LessThanOrEqual => {
                            self.space.cstore.alloc(Box::new(x_leq_y(lhs, rhs)))
                        }
                        expr::BinaryOp::LessThan => {
                            self.space.cstore.alloc(Box::new(XLessY::new(lhs, rhs)))
                        }
                        _ => return Err(SolveError::Internal {
                            msg:
                                "pcp solver: only comparison operators in constraints are expected",
                        }),
                    };
                }
                _ => {
                    return Err(SolveError::Internal {
                        msg: "pcp solver: only binary operators in constraints are expected",
                    })
                }
            }
        }

        // Search step.
        let mut search = one_solution_engine();
        search.start(&self.space);
        let (frozen_space, status) = search.enter(self.space);
        self.status = Some(status);
        self.space = frozen_space.unfreeze();

        Ok(self)
    }

    pub fn print_solution(&self) {
        match self.status {
            Some(Satisfiable) => {
                println!(
                    "   {}",
                    Style::new(Color::Green)
                        .bold()
                        .paint("Problem is satisfiable")
                );
                println!("    {}", Style::new(Color::Green).bold().paint("Solution:"));
                for (idx, dom) in self.space.vstore.iter().enumerate() {
                    println!("     {}: {}", self.intent.vars[idx].name, dom.lower());
                }
            }
            Some(Unsatisfiable) => println!(
                "   {}",
                Style::new(Color::Red)
                    .bold()
                    .paint("Problem is unsatisfiable")
            ),
            Some(EndOfSearch) => println!(
                "   {}",
                Style::new(Color::Red)
                    .bold()
                    .paint("Search terminated or was interrupted")
            ),
            None => println!("Problem has not been solved yet"),
            _ => unreachable!(), // To handle the exhaustive match
        }
    }

    pub fn display_solution_raw(&self) -> String {
        match self.status {
            Some(Satisfiable) => {
                self.space
                    .vstore
                    .iter()
                    .enumerate()
                    .fold(String::new(), |mut acc, (idx, dom)| {
                        writeln!(&mut acc, "{}: {}", self.intent.vars[idx].name, dom.lower())
                            .expect("Failed to write solution to string");
                        acc
                    })
            }
            Some(Unsatisfiable) => "unsatisfiable".to_string(),
            Some(EndOfSearch) => "terminated".to_string(),
            _ => unreachable!(), // To handle the exhaustive match
        }
    }
}
