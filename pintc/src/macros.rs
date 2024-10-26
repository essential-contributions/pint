use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{Expr, Ident, Immediate},
    lexer::Token,
    predicate::{Contract, ExprKey, Param, Predicate},
    span::Span,
    types::UnionDecl,
};

use fxhash::FxHashMap;
use std::{collections::hash_map::Entry, fmt, rc::Rc};

pub(crate) struct MacroDecl {
    pub(crate) name: Ident,
    pub(crate) params: Vec<Ident>,
    pub(crate) pack: Option<Ident>,
    pub(crate) body: Vec<(usize, Token, usize)>,
    pub(crate) sig_span: Span,
}

impl fmt::Display for MacroDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "macro {}(", self.name)?;
        write!(
            f,
            "{}",
            self.params
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        if let Some(pack) = &self.pack {
            write!(f, ", {pack}")?;
        }
        write!(
            f,
            ") {}",
            self.body
                .iter()
                .map(|(_, t, _)| t.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
}

#[derive(Debug)]
pub(crate) struct MacroCall {
    pub(crate) name: String,
    pub(crate) mod_path: Vec<String>,
    pub(crate) args: Vec<Vec<(usize, Token, usize)>>,
    pub(crate) span: Span,
    pub(crate) parent_tag: Option<usize>,
    pub(crate) is_at_decl: bool,
}

impl fmt::Display for MacroCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.name,
            self.args
                .iter()
                .map(|arg| arg
                    .iter()
                    .map(|(_, tok, _)| tok.to_string())
                    .collect::<Vec<_>>()
                    .join(" "))
                .collect::<Vec<_>>()
                .join("; ")
        )
    }
}

pub(crate) fn verify_unique_set(
    handler: &Handler,
    macro_decls: &[MacroDecl],
) -> Result<(), ErrorEmitted> {
    let mut sigs_set = FxHashMap::<_, Span>::default();

    for MacroDecl {
        name,
        params,
        pack,
        sig_span,
        ..
    } in macro_decls
    {
        // The key is name, number of params and whether it has a parameter pack.
        let entry = sigs_set.entry((name.name.clone(), params.len(), pack.is_some()));
        if let Entry::Occupied(prev_span) = entry {
            handler.emit_err(Error::Compile {
                error: CompileError::MacroDeclClash {
                    name: name.name.clone(),
                    span: sig_span.clone(),
                    prev_span: prev_span.get().clone(),
                },
            });
        } else {
            entry.or_insert(sig_span.clone());
        }
    }

    Ok(())
}

pub(crate) fn splice_args(
    handler: &Handler,
    contract: &Contract,
    pred: &Predicate,
    call: &mut MacroCall,
) {
    // Find any args which are spliced identifiers.  Make a list of (arg idx, token idx,
    // identifier name, token span range).
    let mut spliced_args = Vec::new();
    for (arg_idx, arg_tokens) in call.args.iter().enumerate() {
        for (tok_idx, (l, tok, r)) in arg_tokens.iter().enumerate() {
            if let Token::MacroSplice(array_name) = tok {
                spliced_args.push((arg_idx, tok_idx, array_name, *l..*r));
            }
        }
    }

    if spliced_args.is_empty() {
        return;
    }

    let mod_path_str = call
        .mod_path
        .iter()
        .map(|el| format!("::{el}"))
        .collect::<Vec<_>>()
        .concat();

    let mut replacements = FxHashMap::default();
    for (arg_idx, tok_idx, array_name, range) in spliced_args {
        // The identifier will have to be in the same module as the macro call (hence the use of
        // `mod_path_str` above, taken from the call).
        let array_path = mod_path_str.clone() + "::" + array_name;

        if let Some(param_ty) = pred
            .params
            .iter()
            .find_map(|Param { name, ty, .. }| (name.name == array_path).then_some(ty))
        {
            if !param_ty.is_unknown() {
                if let Some(range_expr_key) = param_ty.get_array_range_expr() {
                    if let Some((size, opt_enumeration_union)) =
                        splice_get_array_range_size(contract, range_expr_key)
                    {
                        // Store where and what to replace in the new spliced args.
                        replacements.insert(
                            (arg_idx, tok_idx),
                            (array_name.to_string(), size, opt_enumeration_union, range),
                        );
                    } else {
                        handler.emit_err(Error::Compile {
                            error: CompileError::MacroSpliceArrayUnknownSize {
                                var_name: array_path,
                                span: Span::new(call.span.context(), range),
                            },
                        });
                    }
                } else {
                    handler.emit_err(Error::Compile {
                        error: CompileError::MacroSpliceVarNotArray {
                            var_name: array_path,
                            span: Span::new(call.span.context(), range),
                        },
                    });
                }
            } else {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "missing var type AND init in splice_args()",
                        span: Span::new(call.span.context(), range),
                    },
                });
            }
        } else {
            handler.emit_err(Error::Compile {
                error: CompileError::MacroUnrecognizedSpliceVar {
                    var_name: array_path,
                    span: Span::new(call.span.context(), range),
                },
            });
        }
    }

    // Now rebuild the macro call args by actually splicing in the array(s).  We want to reproduce
    // the Vec<Vec<..>> grid, where each arg is a vector of tokens.
    //
    // Each array element is added as a singular argument.  So, for example, with `ary: int[3]`:
    // - @foo(1; ~ary; 2) becomes @foo(1; ary[0]; ary[1]; ary[2]; 2)
    // - @foo(1 + ~ary; 2) becomes @foo(1 + ary[0]; ary[1]; ary[2]; 2)
    // - @foo(1 + ~ary + 2) becomes @foo(1 + ary[0]; ary[1]; ary[2] + 2)

    let mut new_args = Vec::new();
    for (arg_idx, arg_tokens) in call.args.drain(..).enumerate() {
        new_args.push(Vec::new());

        for (tok_idx, tok) in arg_tokens.into_iter().enumerate() {
            if let Some((name, size, opt_enumeration_union, range)) =
                replacements.get(&(arg_idx, tok_idx))
            {
                // Push an array accessor for every element in the array.  Each token will share
                // the span with the original spliced arg.
                let l = range.start;
                let r = range.end;

                for ary_idx in 0..*size {
                    // If this isn't the very first array access then we need to start a new
                    // argument list.
                    if ary_idx != 0 {
                        new_args.push(Vec::new());
                    }

                    let new_arg_tokens = new_args.last_mut().unwrap();

                    new_arg_tokens.push((l, Token::Ident((name.clone(), true)), r));
                    new_arg_tokens.push((l, Token::BracketOpen, r));
                    if let Some((union_name, variants)) = opt_enumeration_union {
                        // Argh, the union_name already is parsed into a path.
                        new_arg_tokens.push((
                            l,
                            Token::Ident((union_name.name[2..].to_string(), true)),
                            r,
                        ));
                        new_arg_tokens.push((l, Token::DoubleColon, r));
                        new_arg_tokens.push((
                            l,
                            Token::Ident((variants[ary_idx].name.clone(), true)),
                            r,
                        ));
                    } else {
                        new_arg_tokens.push((l, Token::IntLiteral(format!("{ary_idx}")), r));
                    }
                    new_arg_tokens.push((l, Token::BracketClose, r));
                }
            } else {
                // This arg token doesn't need to be replaced.
                new_args.last_mut().unwrap().push(tok);
            }
        }
    }

    call.args = new_args;
}

type OptEnumerationUnionDecl = Option<(Ident, Vec<Ident>)>;

fn splice_get_array_range_size(
    contract: &Contract,
    range_expr_key: ExprKey,
) -> Option<(usize, OptEnumerationUnionDecl)> {
    range_expr_key
        .try_get(contract)
        .and_then(|range_expr| match range_expr {
            Expr::Immediate {
                value: Immediate::Int(size),
                ..
            } => Some((*size as usize, None)),
            Expr::Path(path, _) => contract
                .unions
                .iter()
                .filter(|(_key, union)| union.is_enumeration_union())
                .find_map(|(_, UnionDecl { name, variants, .. })| {
                    (&name.name == path).then(|| {
                        (
                            variants.len(),
                            Some((
                                name.clone(),
                                variants
                                    .iter()
                                    .map(|variant| variant.variant_name.clone())
                                    .collect(),
                            )),
                        )
                    })
                }),

            _ => None,
        })
}

#[derive(Default)]
pub(crate) struct MacroExpander {
    call_history: Vec<(String, usize)>, // Path to macro and number of args.
    call_parents: FxHashMap<usize, usize>, // Indices into self.call_history.
}

impl MacroExpander {
    #[allow(clippy::type_complexity)]
    pub(crate) fn expand_call(
        &mut self,
        handler: &Handler,
        macro_decls: &[MacroDecl],
        call: &MacroCall,
    ) -> Result<(Vec<(usize, Token, usize)>, Span), ErrorEmitted> {
        let macro_decl: &MacroDecl = match_macro(handler, macro_decls, call)?;

        // This tag is for any further calls expanded in this call, to mark their parent.
        let tag = self.call_history.len();

        // Store this 'call sig' -- the name and number of args -- in the call history.
        let call_sig = (call.name.clone(), call.args.len());
        self.call_history.push(call_sig.clone());

        if let Some(parent_tag) = call.parent_tag {
            // Make a link from this call to the parent.
            self.call_parents.insert(tag, parent_tag);

            // Make a local mutable copy for searching the call history.
            let mut parent_tag = parent_tag;

            // Check the history of this call via the parent to find this call sig which would
            // indicate non-terminating recursion.
            loop {
                if self.call_history[parent_tag] == call_sig {
                    handler.emit_err(Error::Compile {
                        error: CompileError::MacroRecursion {
                            name: call.name.clone(),
                            call_span: call.span.clone(),
                            decl_span: macro_decl.sig_span.clone(),
                        },
                    });
                }

                if let Some(grandparent_tag) = self.call_parents.get(&parent_tag) {
                    parent_tag = *grandparent_tag;
                } else {
                    break;
                }
            }
        }

        let param_idcs = FxHashMap::<&String, usize>::from_iter(
            macro_decl
                .params
                .iter()
                .enumerate()
                .map(|(idx, id)| (&id.name, idx)),
        );

        let mut body = Vec::with_capacity(macro_decl.body.len());
        for tok in &macro_decl.body {
            match &tok.1 {
                Token::MacroParam(param) => {
                    if let Some(&idx) = param_idcs.get(param) {
                        // Replace the parameter with the respective call arg tokens.  To avoid the
                        // possibility of wacky spans -- one half being from the args and
                        // essentially 'outside' the macro body and the other half being inside the
                        // macro body -- we give every injected token the span of the param token.
                        body.extend(call.args[idx].iter().map(|(_, arg_tok, _)| {
                            let arg_tok = if let Token::Ident((id, _)) = arg_tok {
                                // When identifiers are passed to a macro we set the special
                                // flag to true to indicate adding a local scope to it should
                                // _not_ be done.  We don't need/want hygiene for these
                                // identifiers.
                                Token::Ident((id.clone(), true))
                            } else {
                                arg_tok.clone()
                            };

                            // Use the span from the MacroParam token.
                            (tok.0, arg_tok, tok.2)
                        }));
                    } else {
                        handler.emit_err(Error::Compile {
                            error: CompileError::MacroUndefinedParam {
                                name: param.clone(),
                                span: Span {
                                    context: Rc::clone(&macro_decl.sig_span.context),
                                    range: tok.0..tok.2,
                                },
                            },
                        });
                    }
                }

                Token::MacroParamPack(pack_name) => {
                    // Confirm the pack has the correct name.
                    if macro_decl
                        .pack
                        .as_ref()
                        .map(|pack_id| &pack_id.name != pack_name)
                        .unwrap_or(true)
                    {
                        handler.emit_err(Error::Compile {
                            error: CompileError::MacroUnknownPack {
                                actual_pack: macro_decl
                                    .pack
                                    .clone()
                                    .map(|pack_id| (pack_id.name, pack_id.span)),
                                bad_pack: (
                                    pack_name.clone(),
                                    Span {
                                        context: Rc::clone(&macro_decl.sig_span.context),
                                        range: tok.0..tok.2,
                                    },
                                ),
                            },
                        });
                    }

                    // `match_macro()` guarantees that the param pack is not empty.  We need to append
                    // the arg tokens separated by `;` for each arg.
                    for arg in call.args.iter().skip(macro_decl.params.len()) {
                        body.append(&mut arg.clone());
                        body.push((0, Token::Semi, 0));
                    }
                }

                Token::MacroTag(empty_tag) => {
                    // This tag has been injected by the lexer.  We update the tag here for when
                    // its associated call is expanded.
                    assert!(empty_tag.is_none());
                    body.push((tok.0, Token::MacroTag(Some(tag)), tok.2))
                }

                _ => body.push(tok.clone()),
            }
        }

        if handler.has_errors() {
            return Err(handler.cancel());
        }

        Ok((body, macro_decl.sig_span.clone()))
    }
}

fn match_macro<'a>(
    handler: &Handler,
    macro_decls: &'a [MacroDecl],
    call: &MacroCall,
) -> Result<&'a MacroDecl, ErrorEmitted> {
    // This method does a lot of validation which will be duplicated for each call.  To avoid this
    // some preprocessing could be done after parsing a module to perform them first.  It is
    // convenient to do them here because we're necessarily collecting all macros by name.

    // Firstly find the macros which have the correct name.
    let mut named_macros = macro_decls
        .iter()
        .filter(|md| md.name.name == call.name)
        .collect::<Vec<_>>();
    if named_macros.is_empty() {
        return Err(handler.emit_err(Error::Compile {
            error: CompileError::MacroNotFound {
                name: call.name.clone(),
                span: call.span.clone(),
            },
        }));
    }

    // Confirm that at most one version has a param pack.
    let pack_spans = named_macros
        .iter()
        .filter_map(|&md| md.pack.as_ref().map(|_| &md.sig_span))
        .collect::<Vec<_>>();
    if pack_spans.len() > 1 {
        return Err(handler.emit_err(Error::Compile {
            // Just take the first two.
            error: CompileError::MacroMultiplePacks {
                span0: pack_spans[0].clone(),
                span1: pack_spans[1].clone(),
            },
        }));
    }

    // Sort them by param count, putting the param pack version last if it exists.
    named_macros.sort_unstable_by(|lhs, rhs| match (lhs.pack.is_some(), rhs.pack.is_some()) {
        (false, false) => lhs.params.len().cmp(&rhs.params.len()),
        (false, true) => std::cmp::Ordering::Less,
        (true, false) => std::cmp::Ordering::Greater,
        (true, true) => unreachable!("We already checked for multiple param packs."),
    });

    // All must be unique counts.
    for pair in named_macros.windows(2) {
        if pair[0].params.len() == pair[1].params.len() && pair[1].pack.is_none() {
            return Err(handler.emit_err(Error::Compile {
                error: CompileError::MacroNonUniqueParamCounts {
                    name: pair[0].name.name.clone(),
                    count: pair[0].params.len(),
                    span0: pair[0].sig_span.clone(),
                    span1: pair[1].sig_span.clone(),
                },
            }));
        }
    }

    // Then search for one with exactly the right number of parameters.
    named_macros
        .iter()
        .find(|md| md.pack.is_none() && md.params.len() == call.args.len())
        .or_else(|| {
            // Or find one with too few parameters AND a parameter pack.
            named_macros
                .iter()
                .find(|md| md.pack.is_some() && md.params.len() < call.args.len())
        })
        .copied()
        .ok_or_else(|| {
            let suggestion = make_comma_semi_suggestion(&named_macros, &call.args);

            handler.emit_err(Error::Compile {
                error: CompileError::MacroCallMismatch {
                    name: call.name.clone(),
                    arg_count: call.args.len(),
                    param_counts_descr: format_param_counts_descr(&named_macros),
                    suggestion,
                    span: call.span.clone(),
                },
            })
        })
}

fn format_param_counts_descr(named_macros: &[&MacroDecl]) -> String {
    use std::fmt::Write;

    fn write_last_count(descr_str: &mut String, md: &MacroDecl) {
        if md.pack.is_none() {
            write!(descr_str, "or {}", md.params.len()).expect("infallible")
        } else {
            write!(descr_str, "or {} or more", md.params.len() + 1).expect("infallible")
        }
    }

    let mut param_counts_descr = String::new();
    match named_macros.len() {
        1 => write!(
            param_counts_descr,
            "exactly {}",
            named_macros[0].params.len()
        )
        .expect("infallible"),

        2 => {
            write!(
                param_counts_descr,
                "either {} ",
                named_macros[0].params.len()
            )
            .expect("infallible");
            write_last_count(&mut param_counts_descr, named_macros[1]);
        }

        _ => {
            // We have at least 3 named macros.
            let (last, middle) = named_macros[1..]
                .split_last()
                .expect("named_macros cannot be empty");
            write!(
                param_counts_descr,
                "either {}",
                named_macros[0].params.len()
            )
            .expect("infallible");
            for md in middle {
                write!(param_counts_descr, ", {}", md.params.len()).expect("infallible");
            }
            write_last_count(&mut param_counts_descr, last);
        }
    }

    param_counts_descr
}

fn make_comma_semi_suggestion(
    macro_decls: &[&MacroDecl],
    args: &[Vec<(usize, Token, usize)>],
) -> Option<String> {
    use std::fmt::Write;

    if macro_decls.len() != 1 {
        // Too hard to make a suggestion if there are multiple call options.
        None
    } else {
        let param_count = macro_decls[0].params.len();
        if param_count > 1 {
            let comma_count = args.iter().fold(0, |acc, arg| {
                acc + arg
                    .iter()
                    .filter(|(_, tok, _)| tok == &Token::Comma)
                    .count()
            });

            if comma_count == param_count - 1 {
                let mut new_args_string = format!(
                    "macro arguments are separated by `;`.  Perhaps try {}(",
                    macro_decls[0].name.name
                );

                for arg in args.iter() {
                    for (_, tok, _) in arg {
                        if tok == &Token::Comma {
                            write!(new_args_string, "; ").unwrap();
                        } else {
                            write!(new_args_string, "{tok}").unwrap();
                        }
                    }
                }
                new_args_string.push(')');

                Some(new_args_string)
            } else {
                None
            }
        } else {
            None
        }
    }
}
