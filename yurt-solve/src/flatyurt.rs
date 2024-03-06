mod display;
pub mod evaluate;
#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct FlatYurt {
    pub decls: Vec<Decl>,
    pub solve: Solve,
}

#[derive(Debug, Clone)]
pub enum Decl {
    Var(Var),
    Constraint(Constraint),
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Constraint(pub Expr);

#[derive(Debug, Clone)]
pub enum Solve {
    Satisfy,
    Minimize(String),
    Maximize(String),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Immediate(Immediate),
    Path(String),
    UnaryOp {
        op: UnaryOp,
        expr: Box<Self>,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
}

#[derive(Debug, Clone)]
pub enum Immediate {
    Bool(bool),
    Int(i64),
    Real(f64),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Comparison
    Equal,
    NotEqual,
    LessThanOrEqual,
    LessThan,
    GreaterThanOrEqual,
    GreaterThan,

    // Logical
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    Int,
    Real,
}
