use crate::{
    error::Error,
    intermediate::{CallKey, ExprKey},
    macros::{MacroCall, MacroDecl},
    parser::{Ident, IntermediateIntent, NextModPath, UsePath},
    span::Span,
};
use std::collections::HashMap;

pub struct ParserContext<'a> {
    pub(crate) mod_path: &'a [String],
    pub(crate) mod_prefix: &'a str,
    pub(crate) local_scope: Option<&'a str>,
    pub(crate) iis: &'a mut HashMap<String, IntermediateIntent>,
    pub(crate) current_ii: &'a mut String,
    pub(crate) macros: &'a mut Vec<MacroDecl>,
    pub(crate) macro_calls: &'a mut slotmap::SecondaryMap<CallKey, (ExprKey, MacroCall)>,
    pub(crate) span_from: &'a dyn Fn(usize, usize) -> Span,
    pub(crate) use_paths: &'a mut Vec<UsePath>,
    pub(crate) next_paths: &'a mut Vec<NextModPath>,
}

impl<'a> ParserContext<'a> {
    pub fn add_top_level_symbol(
        &mut self,
        current_ii: &String,
        mut ident: Ident,
        prefix: &str,
        errors: &mut Vec<Error>,
    ) -> Ident {
        match self.iis.get_mut(current_ii).unwrap().add_top_level_symbol(
            prefix,
            None,
            &ident,
            ident.span.clone(),
        ) {
            Ok(name) => ident.name = name,
            Err(error) => errors.push(Error::Parse { error }),
        }
        ident
    }
}
