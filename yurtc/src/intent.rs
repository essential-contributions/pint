use crate::{ast, error, expr};

mod intermediate;

type Path = String;

/// A checked and verified representation of an intent ready to be solved.
#[derive(Debug)]
pub struct Intent {
    pub states: Vec<State>,
    pub vars: Vec<Variable>,
    pub constraints: Vec<Expression>,
    pub directive: Solve,
}

impl Intent {
    pub(crate) fn from_ast(ast: &[ast::Decl]) -> anyhow::Result<Self> {
        let cnv_err = |e| anyhow::anyhow!(error::Error::Compile { error: e });
        intermediate::IntermediateIntent::from_ast(ast)
            .map_err(cnv_err)?
            .compile()
            .map_err(cnv_err)
    }
}

/// A representation of state fetched from a blockchain.  This is almost always a contract method
/// call or a blockchain attribute call.
#[derive(Debug)]
pub struct State {
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
#[derive(Debug)]
pub enum Expression {
    Immediate(expr::Immediate),
    Ident(Path),
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
#[derive(Debug)]
pub enum Solve {
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
