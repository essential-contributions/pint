use crate::{
    error::Handler,
    intermediate::{CallKey, ExprKey, IntermediateIntent, Program},
    macros::{MacroCall, MacroDecl},
    parser::{Ident, NextModPath, UsePath},
    span::Span,
};
use std::collections::BTreeMap;

pub struct ParserContext<'a> {
    pub(crate) mod_path: &'a [String],
    pub(crate) mod_prefix: &'a str,
    pub(crate) local_scope: Option<&'a str>,
    pub(crate) program: &'a mut Program,
    pub(crate) current_ii: &'a mut String,
    pub(crate) macros: &'a mut Vec<MacroDecl>,
    pub(crate) macro_calls:
        &'a mut BTreeMap<String, slotmap::SecondaryMap<CallKey, (ExprKey, MacroCall)>>,
    pub(crate) span_from: &'a dyn Fn(usize, usize) -> Span,
    pub(crate) use_paths: &'a mut Vec<UsePath>,
    pub(crate) next_paths: &'a mut Vec<NextModPath>,
}

impl<'a> ParserContext<'a> {
    pub fn add_top_level_symbol(
        &mut self,
        handler: &Handler,
        mut ident: Ident,
        prefix: &str,
    ) -> Ident {
        if let Ok(name) = self.current_ii().add_top_level_symbol(
            handler,
            prefix,
            None,
            &ident,
            ident.span.clone(),
        ) {
            ident.name = name
        }
        ident
    }

    /// Returns a mutable reference to the II named `self.current_ii`. Panics if the II cannot be
    /// found, indicating a bug.
    pub fn current_ii(&mut self) -> &mut IntermediateIntent {
        self.program.iis.get_mut(self.current_ii).unwrap()
    }
}
