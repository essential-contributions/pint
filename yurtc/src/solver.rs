use crate::{
    error::SolveError,
    expr,
    intent::{Expression, Intent, Solve, Type},
    span::empty_span,
};
use gcollections::ops::{Alloc, Bounded, Empty};
use interval::{interval_set::IntervalSet, ops::Range};
use pcp::{
    concept::Var,
    kernel::Snapshot,
    propagators::{x_geq_y, x_greater_y, x_leq_y, XEqY, XEqYMulZ, XEqYPlusZ, XLessY, XNeqY},
    search::{
        one_solution_engine,
        search_tree_visitor::Status::{EndOfSearch, Satisfiable, Unsatisfiable},
        FDSpace, Status, VStore,
    },
    term::{Constant, Sum},
    variable::ops::Iterable,
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
    unique_var_suffix: usize, // unique suffix for variables introduced by the solver
    dom_lower: i32,           // Default lower bound for decision variables
    dom_upper: i32,           // Default upper bound for decision variables
}

impl<'a> Solver<'a> {
    /// Creates a new instance of `Solver` given an `Intent`.
    pub fn new(intent: &'a Intent) -> Solver {
        Solver {
            space: FDSpace::empty(),
            variables: Vec::new(),
            intent,
            status: None,
            unique_var_suffix: 0,

            // TODO: The bounds chosen below are a temporary hack. One option is to use the largest
            // possible interval: (i32::MIN, i32::MAX), but that causes some arithmetic overflow
            // issues for some problems. Alternatively, an analysis of the program can lead to
            // better bounds. This needs fixing eventually, but is probably fine for now while we
            // develop the rest of the compiler.
            dom_lower: -1_000_000,
            dom_upper: 1_000_000,
        }
    }

    /// Converts an `intent::Expression` to a `Var<Vstore>`. Works recursively, converting
    /// sub-expressions as needed.
    fn expr_to_vstore(&mut self, expr: &Expression) -> Result<Var<VStore>, SolveError> {
        match expr {
            Expression::Path(path) => self
                .variables
                .iter()
                .find(|v| &v.name == path)
                .map(|SolverVar { var, .. }| var.bclone())
                .ok_or(SolveError::Internal {
                    msg: "(pcp) cannot find variable for path expression",
                    span: empty_span(),
                }),
            Expression::Immediate(expr::Immediate::Int(val)) => Ok(Box::new(Constant::new(
                i32::try_from(*val).map_err(|_| SolveError::Internal {
                    msg: "(pcp) integer immediate is too large for the solver",
                    span: empty_span(),
                })?,
            ))),
            Expression::BinaryOp { op, lhs, rhs } => {
                let lhs = self.expr_to_vstore(lhs)?;
                let rhs = self.expr_to_vstore(rhs)?;
                match op {
                    expr::BinaryOp::Add => Ok(Box::new(Sum::new(vec![lhs, rhs]))),
                    expr::BinaryOp::Sub => {
                        // Introduce a new variable with a unique name, constrain it with `lhs ==
                        // rhs + <new_var>`, and then return it.
                        let sub_var = Box::new(
                            self.space
                                .vstore
                                .alloc(IntervalSet::new(self.dom_lower, self.dom_upper)),
                        ) as Var<VStore>;
                        self.space.cstore.alloc(Box::new(XEqYPlusZ::new(
                            lhs,
                            rhs,
                            sub_var.bclone(),
                        )));
                        self.variables.push(SolverVar {
                            name: format!("INTRODUCED{}", self.unique_var_suffix),
                            var: sub_var.bclone(),
                        });
                        self.unique_var_suffix += 1;
                        Ok(sub_var)
                    }
                    expr::BinaryOp::Mul => {
                        // Introduce a new variable with a unique name, constrain it to be equal to
                        // `lhs * rhs`, and then return it.
                        let mul_var = Box::new(
                            self.space
                                .vstore
                                .alloc(IntervalSet::new(self.dom_lower, self.dom_upper)),
                        ) as Var<VStore>;
                        self.space.cstore.alloc(Box::new(XEqYMulZ::new(
                            mul_var.bclone(),
                            lhs,
                            rhs,
                        )));
                        self.variables.push(SolverVar {
                            name: format!("INTRODUCED{}", self.unique_var_suffix),
                            var: mul_var.bclone(),
                        });
                        self.unique_var_suffix += 1;
                        Ok(mul_var)
                    }
                    expr::BinaryOp::Div => {
                        // Introduce a new variable with a unique name, constrain it with
                        // `lhs == rhs * <new_var> `, and then return it.
                        let div_var = Box::new(
                            self.space
                                .vstore
                                .alloc(IntervalSet::new(self.dom_lower, self.dom_upper)),
                        ) as Var<VStore>;
                        self.space.cstore.alloc(Box::new(XEqYMulZ::new(
                            lhs,
                            rhs,
                            div_var.bclone(),
                        )));
                        self.variables.push(SolverVar {
                            name: format!("INTRODUCED{}", self.unique_var_suffix),
                            var: div_var.bclone(),
                        });
                        self.unique_var_suffix += 1;
                        Ok(div_var)
                    }
                    _ => Err(SolveError::Internal {
                        msg: "(pcp) only addition is expected in expressions",
                        span: empty_span(),
                    }),
                }
            }
            _ => Err(SolveError::Internal {
                msg: "(pcp) only paths, immediates, and binary ops are expected in expressions",
                span: empty_span(),
            }),
        }
    }

    /// Produces a solution to the intent:
    /// - Updates the domain bounds of `self.variables` upon success
    /// - Updates `self.status` with the status of the problem (Satisfiable v.s. Unsatisfiable)
    pub fn solve(mut self) -> Result<Self, SolveError> {
        // Optimization problems are not supported yet
        if !matches!(self.intent.directive, Solve::Satisfy) {
            return Err(SolveError::Internal {
                msg: "(pcp) only constraint satisfaction problems are currently supported",
                span: empty_span(),
            });
        }

        // No state variables are allowed
        if !self.intent.states.is_empty() {
            return Err(SolveError::Internal {
                msg: "(pcp) no state variables are allowed at this stage",
                span: empty_span(),
            });
        }

        // Only integer variables are currently supported
        if self.intent.vars.iter().any(|v| !matches!(v.ty, Type::Int)) {
            return Err(SolveError::Internal {
                msg: "(pcp) only integer variables are currently supported",
                span: empty_span(),
            });
        }

        // Convert all variables
        for v in &self.intent.vars {
            self.variables.push(SolverVar {
                name: v.name.clone(),
                var: Box::new(
                    self.space
                        .vstore
                        .alloc(IntervalSet::new(self.dom_lower, self.dom_upper)),
                ) as Var<VStore>,
            });
        }

        // Convert all constraints
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
                        _ => {
                            return Err(SolveError::Internal {
                                msg: "(pcp) only comparison operators in constraints are expected",
                                span: empty_span(),
                            })
                        }
                    };
                }
                _ => {
                    return Err(SolveError::Internal {
                        msg: "(pcp) only binary operators in constraints are expected",
                        span: empty_span(),
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

    /// Pretty print the output of the solver which includes a valid solution for all the variables
    /// in case of success. Skips any helper variables introduced by the solver.
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
                    // This assumes that the original variables are always added first, followed by
                    // the list of helper variables introduced by the solver. Hence, only print the
                    // first `self.intent.vars.len()` variables.
                    if idx < self.intent.vars.len() {
                        println!("     {}: {}", self.variables[idx].name, dom.lower());
                    }
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

    /// Serialize the raw output of the solver into a `String`. This is mostly meant for testing
    /// purposes. Includes helper variables introduced by the solver.
    pub fn display_solution_raw(&self) -> String {
        match self.status {
            Some(Satisfiable) => {
                self.space
                    .vstore
                    .iter()
                    .enumerate()
                    .fold(String::new(), |mut acc, (idx, dom)| {
                        writeln!(&mut acc, "{}: {}", self.variables[idx].name, dom.lower())
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
