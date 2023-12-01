use crate::{
    intent::intermediate::CallKey,
    macros::{MacroCall, MacroDecl},
    parser::{IntermediateIntent, NextModPath, UsePath},
    span::Span,
};

pub struct ParserContext<'a> {
    pub(crate) mod_path: &'a [String],
    pub(crate) mod_prefix: &'a str,
    pub(crate) ii: &'a mut IntermediateIntent,
    pub(crate) macros: &'a mut Vec<MacroDecl>,
    pub(crate) macro_calls: &'a mut slotmap::SecondaryMap<CallKey, MacroCall>,
    pub(crate) span_from: &'a dyn Fn(usize, usize) -> Span,
    pub(crate) use_paths: &'a mut Vec<UsePath>,
    pub(crate) next_paths: &'a mut Vec<NextModPath>,
}
