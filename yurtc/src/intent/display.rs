use crate::{
    expr,
    intent::{Expression, Solve, State, Type, Variable},
    util::write_many,
};
use std::fmt::{Display, Formatter, Result};

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "var {}: {}", self.name, self.ty)?;
        Ok(())
    }
}

impl Display for State {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "state {}: {} = {}", self.name, self.ty, self.expr)?;
        Ok(())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "{}",
            match self {
                Type::Bool => "bool",
                Type::Int => "int",
                Type::Real => "real",
                Type::String => "string",
            }
        )?;
        Ok(())
    }
}

impl Display for Solve {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "solve ")?;
        match self {
            Solve::Satisfy => write!(f, "satisfy"),
            Solve::Minimize(expr) => write!(f, "minimize {expr}"),
            Solve::Maximize(expr) => write!(f, "maximize {expr}"),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Expression::Immediate(imm) => write!(f, "{imm}"),
            Expression::Path(path) => write!(f, "{path}"),
            Expression::UnaryOp { op, expr, .. } => {
                if matches!(op, expr::UnaryOp::NextState) {
                    write!(f, "{expr}'")
                } else {
                    match op {
                        expr::UnaryOp::Error => write!(f, "error"),
                        expr::UnaryOp::Neg => write!(f, "-"),
                        expr::UnaryOp::Not => write!(f, "!"),
                        expr::UnaryOp::NextState => unreachable!(),
                    }?;
                    write!(f, "{expr}")
                }
            }
            Expression::BinaryOp { op, lhs, rhs, .. } => {
                write!(f, "({lhs} {op} {rhs})")
            }

            Expression::Call { name, args } => {
                write!(f, "{name}(")?;
                write_many!(f, args, ", ");
                write!(f, ")")
            }

            Expression::If {
                condition,
                then_expr,
                else_expr,
                ..
            } => write!(
                f,
                "if {} {{ {} }} else {{ {} }}",
                *condition, *then_expr, *else_expr
            ),
        }
    }
}

impl Display for super::Intent {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for var in &self.vars {
            writeln!(f, "{var};")?;
        }
        for state in &self.states {
            writeln!(f, "{state};")?;
        }
        for constraint in &self.constraints {
            writeln!(f, "constraint {constraint};")?;
        }
        writeln!(f, "{};", self.directive)?;
        Ok(())
    }
}
