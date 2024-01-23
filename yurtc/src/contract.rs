use crate::{
    expr::Ident,
    intent::intermediate::{DisplayWithII, ExprKey, IntermediateIntent, State},
    span::{Span, Spanned},
    types::{MsgSig, Path},
    util::write_many,
};
use std::fmt::{Formatter, Result};

#[derive(Clone, Debug, PartialEq)]
pub struct InterfaceDecl {
    pub(super) name: Ident,
    pub(super) msg_sigs: Vec<MsgSig>,
    pub(super) span: Span,
}

impl Spanned for InterfaceDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl DisplayWithII for InterfaceDecl {
    fn fmt(&self, f: &mut Formatter<'_>, ii: &IntermediateIntent) -> Result {
        write!(f, "interface {} {{ ", self.name)?;
        for msg_sig in &self.msg_sigs {
            write!(f, "{}; ", ii.with_ii(msg_sig))?;
        }
        write!(f, "}}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum MsgStatement {
    Constraint(ExprKey),
    State(State),
}

impl DisplayWithII for MsgStatement {
    fn fmt(&self, f: &mut Formatter<'_>, ii: &IntermediateIntent) -> Result {
        match self {
            MsgStatement::Constraint(constraint) => {
                write!(f, "constraint {}", ii.with_ii(constraint))
            }
            MsgStatement::State(state) => {
                write!(f, "{}", ii.with_ii(state))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MsgDecl {
    pub(super) msg_sig: MsgSig,
    pub(super) statements: Vec<MsgStatement>,
    pub(super) span: Span,
}

impl DisplayWithII for MsgDecl {
    fn fmt(&self, f: &mut Formatter<'_>, ii: &IntermediateIntent) -> Result {
        write!(f, "{}", ii.with_ii(self.msg_sig.clone()))?;
        write!(f, " {{ ")?;
        for statement in &self.statements {
            write!(f, "{}; ", ii.with_ii(statement))?;
        }
        write!(f, "}}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ContractDecl {
    pub(super) name: Ident,
    pub(super) interfaces: Vec<Path>,
    pub(super) msg_decls: Vec<MsgDecl>,
    pub(super) span: Span,
}

impl Spanned for ContractDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl DisplayWithII for ContractDecl {
    fn fmt(&self, f: &mut Formatter<'_>, ii: &IntermediateIntent) -> Result {
        write!(f, "contract {}", self.name)?;
        if !self.interfaces.is_empty() {
            write!(f, " implements ")?;
            write_many!(f, self.interfaces, ", ");
        }
        write!(f, " {{ ")?;
        for msg_decl in &self.msg_decls {
            write!(f, "{} ", ii.with_ii(msg_decl))?;
        }
        write!(f, "}}")
    }
}
