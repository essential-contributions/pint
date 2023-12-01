use crate::{expr::Ident, lexer::Token, types::Path};

use std::fmt;

pub(crate) struct MacroDecl<'sc> {
    pub(crate) name: Ident,
    pub(crate) params: Vec<Ident>,
    pub(crate) body: Vec<Token<'sc>>,
}

impl<'sc> fmt::Display for MacroDecl<'sc> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "macro {}({}) {}",
            self.name,
            self.params
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<_>>()
                .join(", "),
            self.body
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
}
pub(crate) struct MacroCall<'sc> {
    pub(crate) name: Path,
    pub(crate) args: Vec<Vec<Token<'sc>>>,
}

impl<'sc> fmt::Display for MacroCall<'sc> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.name,
            self.args
                .iter()
                .map(|arg| arg
                    .iter()
                    .map(|tok| tok.to_string())
                    .collect::<Vec<_>>()
                    .join(" "))
                .collect::<Vec<_>>()
                .join("; ")
        )
    }
}
