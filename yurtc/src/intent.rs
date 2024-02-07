use crate::expr;

pub(crate) mod intermediate;
pub use intermediate::IntermediateIntent;

pub mod display;

type Path = String;

/// A checked and verified representation of an intent ready to be solved.
#[derive(Debug)]
pub struct Intent {
    pub states: Vec<StateVar>,
    pub vars: Vec<Variable>,
    pub constraints: Vec<Expression>,
    pub directive: SolveDirective,
}

/// A representation of state fetched from a blockchain.  This is almost always a contract method
/// call or a blockchain attribute call.
#[derive(Debug)]
pub struct StateVar {
    pub name: Path,
    pub ty: Type,
    pub expr: Expression,
}

/// A decision variable.  A typed symbol whose value is to be resolved.
#[derive(Debug)]
pub struct Variable {
    pub name: Path,
    pub ty: Type,
}

/// An expression describing the possible values for one or more [Variable]s or [State]s.
/// NOTE: We don't _yet_ have a way to represent contract calls properly here.
#[derive(Clone, Debug)]
pub enum Expression {
    Immediate(expr::Immediate),
    Path(Path),
    UnaryOp {
        op: expr::UnaryOp,
        expr: Box<Self>,
    },
    BinaryOp {
        op: expr::BinaryOp,
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    Call {
        name: Path,
        args: Vec<Self>,
    },
    If {
        condition: Box<Self>,
        then_expr: Box<Self>,
        else_expr: Box<Self>,
    },
}

/// A directive describing in particular which [Variable] to resolve and how.
///
/// Yurt does support arbitrary objective functions, but here we assume that the objective function
/// has been replaced by a single new variable that is constrained to be equal to the objective
/// function. This transformation has to be done by the compiler.
///
/// For example:
///
/// ```yurt
/// maximize x + y
/// ```
///
/// becomes
///
/// ```yurt
/// let z = x + y;
/// maximize z
/// ```
#[derive(Debug)]
pub enum SolveDirective {
    /// Resolve all [Variable]s.
    Satisfy,
    /// Resolve to minimize a particular named [Variable].
    Minimize(Path),
    /// Resolve to maximize a particular named [Variable].
    Maximize(Path),
}

/// The type of a [Variable].
#[derive(Debug)]
pub enum Type {
    Bool,
    Int,
    Real,
    String,
}
