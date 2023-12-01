use crate::{expr::Ident, lexer::Token};

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
