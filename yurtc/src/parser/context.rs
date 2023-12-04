use crate::{
    error::Error,
    intent::intermediate::{CallKey, ExprKey},
    macros::{MacroCall, MacroDecl},
    parser::Ident,
    parser::{IntermediateIntent, NextModPath, UsePath},
    span::Span,
};

pub struct ParserContext<'a> {
    pub(crate) mod_path: &'a [String],
    pub(crate) mod_prefix: &'a str,
    pub(crate) local_scope: Option<&'a str>,
    pub(crate) ii: &'a mut IntermediateIntent,
    pub(crate) macros: &'a mut Vec<MacroDecl>,
    pub(crate) macro_calls: &'a mut slotmap::SecondaryMap<CallKey, (ExprKey, MacroCall)>,
    pub(crate) span_from: &'a dyn Fn(usize, usize) -> Span,
    pub(crate) use_paths: &'a mut Vec<UsePath>,
    pub(crate) next_paths: &'a mut Vec<NextModPath>,
}

impl<'a> ParserContext<'a> {
    pub fn add_top_level_symbol(
        &mut self,
        mut ident: Ident,
        prefix: &str,
        errors: &mut Vec<Error>,
    ) -> Ident {
        match self
            .ii
            .add_top_level_symbol(prefix, None, &ident, ident.span.clone())
        {
            Ok(name) => ident.name = name,
            Err(error) => errors.push(Error::Parse { error }),
        }
        ident
    }
}
