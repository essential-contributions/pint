use crate::{
    error::SolveError,
    expr,
    intent::{self, Expression, Intent, SolveDirective, Type},
    span::empty_span,
};
use russcip::{prelude::*, ProblemCreated, Solved, Variable};
use std::{fmt::Write, rc::Rc};
use yansi::{Color, Style};

pub struct Solver<'a, State> {
    model: Model<State>,
    intent: &'a Intent,
    unique_var_suffix: usize, // unique suffix for variables introduced by the solver
    unique_cons_suffix: usize, // unique suffix for names of cosntraints
}

impl<'a, State> Solver<'a, State> {
    /// Creates a new instance of `Solver` given an `Intent`.
    pub fn new(intent: &'a Intent) -> Solver<ProblemCreated> {
        Solver {
            model: Model::new()
                .hide_output()
                .include_default_plugins()
                .create_prob("solver")
                .set_obj_sense(match intent.directive {
                    // For constraint satisfaction problems, `ObjSense` does not matter. The
                    // objective function is going to be set to 0 anyways.
                    SolveDirective::Minimize(_) | SolveDirective::Satisfy => ObjSense::Minimize,
                    SolveDirective::Maximize(_) => ObjSense::Maximize,
                }),
            intent,
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
    /// Converts an `intent::Expression` to an `Rc<Variable>`. Works recursively, converting
    /// sub-expressions as needed.
    fn expr_to_var(&mut self, expr: &Expression) -> Result<Rc<Variable>, SolveError> {
        match expr {
            Expression::Path(path) => self
                .model
                .vars()
                .iter()
                .find(|v| &v.name() == path)
                .ok_or(SolveError::Internal {
                    msg: "(scip) cannot find variable for path expression",
                    span: empty_span(),
                })
                .cloned(),

            Expression::Immediate(imm) => match imm {
                expr::Immediate::Real(val) => {
                    let new_var_name = self.new_var_name();
                    Ok(self
                        .model
                        .add_var(*val, *val, 0., &new_var_name, VarType::Continuous))
                }
                expr::Immediate::Int(val) => {
                    let new_var_name = self.new_var_name();
                    Ok(self.model.add_var(
                        *val as f64,
                        *val as f64,
                        0.,
                        &new_var_name,
                        VarType::Integer,
                    ))
                }
                _ => Err(SolveError::Internal {
                    msg: "(scip) only integer and real immediates are currently supported",
                    span: empty_span(),
                }),
            },

            Expression::BinaryOp { op, lhs, rhs } => {
                // Add an auxilliary variable constrain it to be equal to the result of the binary
                // operation.
                let lhs = self.expr_to_var(lhs)?;
                let rhs = self.expr_to_var(rhs)?;
                let lhs_type = lhs.var_type();
                let rhs_type = rhs.var_type();

                let new_var_name = self.new_var_name();
                let new_var_type = match (lhs_type, rhs_type) {
                    (VarType::Integer | VarType::Binary, VarType::Integer | VarType::Binary) => {
                        VarType::Integer
                    }
                    (VarType::Continuous, VarType::Continuous) => VarType::Continuous,
                    _ => {
                        return Err(SolveError::Internal {
                            msg: "(scip) incompatible types encountered",
                            span: empty_span(),
                        });
                    }
                };

                let new_cons_name = self.new_cons_name();

                match op {
                    expr::BinaryOp::Add => {
                        let add_var = self.model.add_var(
                            lhs.lb() + rhs.lb(),
                            lhs.ub() + rhs.ub(),
                            0.,
                            &new_var_name,
                            new_var_type,
                        );
                        // constraint lhs + rhs - add_var == 0
                        self.model.add_cons(
                            vec![lhs.clone(), rhs.clone(), add_var.clone()],
                            &[1., 1., -1.],
                            0.,
                            0.,
                            &new_cons_name,
                        );
                        Ok(add_var)
                    }
                    expr::BinaryOp::Sub => {
                        let sub_var = self.model.add_var(
                            lhs.lb() - rhs.ub(),
                            lhs.ub() - rhs.lb(),
                            0.,
                            &new_var_name,
                            new_var_type,
                        );
                        // constraint lhs - rhs - sub_var == 0
                        self.model.add_cons(
                            vec![lhs.clone(), rhs.clone(), sub_var.clone()],
                            &[1., -1., -1.],
                            0.,
                            0.,
                            &new_cons_name,
                        );
                        Ok(sub_var)
                    }
                    expr::BinaryOp::Mul => {
                        let bounds = [
                            lhs.lb() * rhs.lb(),
                            lhs.lb() * rhs.ub(),
                            lhs.ub() * rhs.lb(),
                            lhs.ub() * rhs.ub(),
                        ];
                        let mult_var = self.model.add_var(
                            *bounds
                                .iter()
                                .min_by(|x, y| x.partial_cmp(y).unwrap())
                                .unwrap(),
                            *bounds
                                .iter()
                                .max_by(|x, y| x.partial_cmp(y).unwrap())
                                .unwrap(),
                            0.,
                            &new_var_name,
                            new_var_type,
                        );
                        // constraint lhs * rhs - mult_var == 0
                        self.model.add_cons_quadratic(
                            vec![mult_var.clone()],
                            &mut [-1.],
                            vec![lhs.clone()],
                            vec![rhs.clone()],
                            &mut [1.],
                            0.,
                            0.,
                            &new_cons_name,
                        );
                        Ok(mult_var)
                    }
                    expr::BinaryOp::Div => {
                        // TODO: compute better bounds for this variable
                        let div_var = self.model.add_var(
                            -f64::INFINITY,
                            f64::INFINITY,
                            0.,
                            &new_var_name,
                            new_var_type,
                        );
                        // constraint div_var * rhs - lhs == 0
                        self.model.add_cons_quadratic(
                            vec![lhs.clone()],
                            &mut [-1.],
                            vec![div_var.clone()],
                            vec![rhs.clone()],
                            &mut [1.],
                            0.,
                            0.,
                            &new_cons_name,
                        );
                        Ok(div_var)
                    }
                    _ => Err(SolveError::Internal {
                        msg: "(scip) only addition is expected in expressions",
                        span: empty_span(),
                    }),
                }
            }

            Expression::UnaryOp { op, expr } => {
                let expr = self.expr_to_var(expr)?;
                let new_var_name = self.new_var_name();
                let new_cons_name = self.new_cons_name();

                match op {
                    expr::UnaryOp::Neg => {
                        let neg_var = self.model.add_var(
                            -expr.ub(),
                            -expr.lb(),
                            0.,
                            &new_var_name,
                            expr.var_type(),
                        );
                        // constraint neg_var + expr == 0
                        self.model.add_cons(
                            vec![neg_var.clone(), expr.clone()],
                            &[1., 1.],
                            0.,
                            0.,
                            &new_cons_name,
                        );
                        Ok(neg_var)
                    }
                    _ => Err(SolveError::Internal {
                        msg: "(scip) unsupported unary operator",
                        span: empty_span(),
                    }),
                }
            }
            _ => Err(SolveError::Internal {
                msg: "(scip) only paths, immediates, and binary ops are expected in expressions",
                span: empty_span(),
            }),
        }
    }

    pub fn solve(mut self) -> Result<Solver<'a, Solved>, SolveError> {
        // No state variables are allowed
        if !self.intent.states.is_empty() {
            return Err(SolveError::Internal {
                msg: "(scip) no state variables are allowed at this stage",
                span: empty_span(),
            });
        }

        // Convert all variables
        for variable in &self.intent.vars {
            self.convert_variable(variable)?;
        }

        // Convert all constraints
        for constraint in &self.intent.constraints {
            self.convert_constraint(constraint)?;
        }

        Ok(Solver {
            model: self.model.solve(),
            intent: self.intent,
            unique_var_suffix: self.unique_var_suffix,
            unique_cons_suffix: self.unique_cons_suffix,
        })
    }

    fn convert_variable(&mut self, variable: &intent::Variable) -> Result<(), SolveError> {
        self.model.add_var(
            -f64::INFINITY, // TODO: We can do better with a proper analysis
            f64::INFINITY,  // TODO: We can do better with a proper analysis
            match &self.intent.directive {
                SolveDirective::Satisfy => 0.,
                SolveDirective::Minimize(path) | SolveDirective::Maximize(path) => {
                    if path == &variable.name {
                        1.
                    } else {
                        0.
                    }
                }
            },
            &variable.name.clone(),
            match variable.ty {
                Type::Bool => VarType::Binary,
                Type::Int => VarType::Integer,
                Type::Real => VarType::Continuous,
                Type::String => {
                    return Err(SolveError::Internal {
                        msg: "(scip) string types are not yet supported",
                        span: empty_span(),
                    })
                }
            },
        );
        Ok(())
    }

    fn convert_constraint(&mut self, constraint: &Expression) -> Result<(), SolveError> {
        let new_cons_name = self.new_cons_name();
        match constraint {
            Expression::BinaryOp { op, lhs, rhs } => {
                let lhs = self.expr_to_var(lhs)?;
                let rhs = self.expr_to_var(rhs)?;
                let lhs_type = lhs.var_type();
                let rhs_type = rhs.var_type();
                match op {
                    expr::BinaryOp::Equal => {
                        // 0 <= lhs - rhs <= 0
                        self.model.add_cons(
                            vec![lhs.clone(), rhs.clone()],
                            &[1., -1.],
                            0.,
                            0.,
                            &new_cons_name,
                        );
                    }
                    expr::BinaryOp::NotEqual => {
                        if lhs_type != VarType::Continuous && rhs_type != VarType::Continuous {
                            // Decides whether `lhs > rhs` or lhs < rhs`
                            let new_var_name = self.new_var_name();
                            let boolean =
                                self.model
                                    .add_var(0., 1., 0., &new_var_name, VarType::Binary);

                            // 2*lhs*b - 2*rhs*b - lhs + rhs >= 1
                            // If b = 0, we get: rhs >= lhs + 1
                            // If b = 1, we get: lhs >= rhs + 1
                            self.model.add_cons_quadratic(
                                vec![lhs.clone(), rhs.clone()],
                                &mut [-1., 1.],
                                vec![lhs.clone(), rhs.clone()],
                                vec![boolean.clone(), boolean.clone()],
                                &mut [2., -2.],
                                1.,
                                f64::INFINITY,
                                &new_cons_name,
                            );

                            // TODO: can we do better? The above is fine for now but is a bit
                            // expensive. Typical linearization techniques rely on a "large
                            // enough" value `M` used as follows:
                            // ```
                            // constraint lhs - rhs <= -1 + M*b
                            // constraint -lhs + rhs <= -1 + M*(1 - b)
                            // ```
                            // but choosing an appropriate `M` is not trivial and is likely
                            // dependent on the problem at hand.
                        } else {
                            return Err(SolveError::Internal {
                                msg:
                                    "(scip) not equal constraint is not supported for non-integers",
                                span: empty_span(),
                            });
                        }
                    }
                    expr::BinaryOp::GreaterThanOrEqual => {
                        // 0 <= lhs - rhs <= f64::INFINITY
                        self.model.add_cons(
                            vec![lhs.clone(), rhs.clone()],
                            &[1., -1.],
                            0.,
                            f64::INFINITY,
                            &new_cons_name,
                        );
                    }
                    expr::BinaryOp::GreaterThan => {
                        if lhs_type != VarType::Continuous && rhs_type != VarType::Continuous {
                            // 1 <= lhs - rhs <= f64::INFINITY
                            self.model.add_cons(
                                vec![lhs.clone(), rhs.clone()],
                                &[1., -1.],
                                1.,
                                f64::INFINITY,
                                &new_cons_name,
                            );
                        } else {
                            return Err(SolveError::Internal {
                                msg: "(scip) strict inequalities not supported for non-integers",
                                span: empty_span(),
                            });
                        }
                    }
                    expr::BinaryOp::LessThanOrEqual => {
                        // 0 <= -lhs + rhs <= f64::INFINITY
                        self.model.add_cons(
                            vec![lhs.clone(), rhs.clone()],
                            &[-1., 1.],
                            0.,
                            f64::INFINITY,
                            &new_cons_name,
                        );
                    }
                    expr::BinaryOp::LessThan => {
                        if lhs_type != VarType::Continuous && rhs_type != VarType::Continuous {
                            // 1 <= -lhs + rhs <= f64::INFINITY
                            self.model.add_cons(
                                vec![lhs.clone(), rhs.clone()],
                                &[-1., 1.],
                                1.,
                                f64::INFINITY,
                                &new_cons_name,
                            );
                        } else {
                            return Err(SolveError::Internal {
                                msg: "(scip) strict inequalities not supported for non-integers",
                                span: empty_span(),
                            });
                        }
                    }
                    _ => {
                        return Err(SolveError::Internal {
                            msg: "(scip) only comparison operators in constraints are expected",
                            span: empty_span(),
                        })
                    }
                }
            }
            _ => {
                return Err(SolveError::Internal {
                    msg: "(scip) only binary operators in constraints are expected",
                    span: empty_span(),
                })
            }
        }
        Ok(())
    }
}

impl<'a> Solver<'a, Solved> {
    /// Pretty print the output of the solver which includes a valid solution for all the variables
    /// in case of success. Skips any helper variables introduced by the solver.
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
                        println!("     {}: {}", &var.name(), sol.val(var.clone()));
                    });

                if !matches!(self.intent.directive, SolveDirective::Satisfy) {
                    println!(
                        "    {}: {}",
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
    /// purposes. Includes helper variables introduced by the solver.
    pub fn display_solution_raw(&self) -> String {
        match self.model.status() {
            Status::Optimal => {
                let sol = self.model.best_sol().unwrap();
                let solution = self.model.vars().iter().take(self.intent.vars.len()).fold(
                    String::new(),
                    |mut acc, var| {
                        writeln!(&mut acc, "{}: {}", &var.name(), sol.val(var.clone()))
                            .expect("Failed to write solution to string");
                        acc
                    },
                );
                if matches!(self.intent.directive, SolveDirective::Satisfy) {
                    solution
                } else {
                    format!("{solution}objective: {}", self.model.obj_val())
                }
            }
            Status::Infeasible => "infeasible".to_string(),
            Status::Unbounded => "unbounded".to_string(),
            Status::Inforunbd => "infeasible or unbounded".to_string(),
            _ => format!("scip_status({:?})", self.model.status()),
        }
    }
}
