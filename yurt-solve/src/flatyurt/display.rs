use super::{BinaryOp, Constraint, Decl, Expr, FlatYurt, Immediate, Solve, Type, UnaryOp, Var};
use std::fmt::{Display, Formatter, Result};
impl Display for FlatYurt {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for decl in &self.decls {
            writeln!(f, "{decl}")?;
        }
        writeln!(f, "{}", self.solve)
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Decl::Var(var) => write!(f, "{var}"),
            Decl::Constraint(constraint) => write!(f, "{constraint}"),
        }
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "var {}: {};", self.name, self.ty)
    }
}

impl Display for Constraint {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "constraint {};", self.0)
    }
}

impl Display for Solve {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Solve::Satisfy => write!(f, "solve satisfy;"),
            Solve::Minimize(obj) => write!(f, "solve minimize {obj};"),
            Solve::Maximize(obj) => write!(f, "solve maximize {obj};"),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Expr::Immediate(imm) => write!(f, "{imm}"),
            Expr::Path(path) => write!(f, "{path}"),
            Expr::UnaryOp { op, expr } => write!(f, "{op}{expr}"),
            Expr::BinaryOp { op, lhs, rhs } => write!(f, "({lhs} {op} {rhs})"),
        }
    }
}

impl Display for Immediate {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Immediate::Bool(b) => write!(f, "{b}"),
            Immediate::Int(i) => write!(f, "{i}"),
            Immediate::Real(r) => write!(f, "{r:e}"),
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Mod => write!(f, "%"),
            BinaryOp::Equal => write!(f, "=="),
            BinaryOp::NotEqual => write!(f, "!="),
            BinaryOp::LessThanOrEqual => write!(f, "<="),
            BinaryOp::LessThan => write!(f, "<"),
            BinaryOp::GreaterThanOrEqual => write!(f, ">="),
            BinaryOp::GreaterThan => write!(f, ">"),
            BinaryOp::LogicalAnd => write!(f, "&&"),
            BinaryOp::LogicalOr => write!(f, "||"),
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Real => write!(f, "real"),
        }
    }
}
