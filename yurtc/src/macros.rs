use crate::{
    error::{CompileError, Error},
    expr::Ident,
    lexer::Token,
    span::Span,
    types::Path,
};

use std::{
    collections::{hash_map::Entry, HashMap},
    fmt,
    rc::Rc,
};

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

pub(crate) struct MacroCall {
    pub(crate) name: Path,
    pub(crate) mod_path: Vec<String>,
    pub(crate) args: Vec<Vec<(usize, Token, usize)>>,
    pub(crate) span: Span,
    pub(crate) tag: Option<u64>,
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

pub(crate) fn verify_unique_set(macro_decls: &[MacroDecl]) -> Vec<Error> {
    let mut errors = Vec::new();
    let mut sigs_set = HashMap::<_, Span>::new();

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
            errors.push(Error::Compile {
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

    errors
}

#[derive(Default)]
pub(crate) struct MacroExpander {
    call_histories: HashMap<u64, Vec<(Path, usize)>>,
    next_tag: u64,
}

impl MacroExpander {
    pub(crate) fn expand_call(
        &mut self,
        macro_decls: &[MacroDecl],
        call: &MacroCall,
    ) -> Result<(Vec<(usize, Token, usize)>, Span), Vec<Error>> {
        let macro_decl: &MacroDecl = match_macro(macro_decls, call)?;
        let mut errs = Vec::new();

        // This tag identifies the current expansion chain.  Root calls from outside of macro
        // bodies will be None and so we allocate a new unique tag.  Calls in macro bodies have
        // this tag propogated and we track the expansion history to detect non-terminating
        // recursion.
        let tag = call.tag.unwrap_or_else(|| {
            let tag = self.next_tag;
            self.next_tag += 1;
            tag
        });

        // If this 'call sig' -- the name and number of args -- has been used in this expansion
        // chain before then we have illegal recursion.
        let call_sig = (call.name.clone(), call.args.len());
        self.call_histories
            .entry(tag)
            .and_modify(|call_history| {
                if call_history.contains(&call_sig) {
                    errs.push(Error::Compile {
                        error: CompileError::MacroRecursion {
                            name: call.name.clone(),
                            call_span: call.span.clone(),
                            decl_span: macro_decl.sig_span.clone(),
                        },
                    });
                }
                call_history.push(call_sig.clone())
            })
            .or_insert(vec![call_sig]);

        let param_idcs = HashMap::<&String, usize>::from_iter(
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
                        body.append(&mut call.args[idx].clone());
                    } else {
                        errs.push(Error::Compile {
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

                Token::MacroParamPack(_) => {
                    // `match_macro()` guarantees that the param pack is not empty.  We need to append
                    // the arg tokens separated by `;` for each arg.
                    for arg in call.args.iter().skip(macro_decl.params.len()) {
                        body.append(&mut arg.clone());
                        body.push((0, Token::Semi, 0));
                    }
                }

                Token::MacroTag(old_tag) => {
                    // This tag has been injected by the lexer.  We update the tag here for when
                    // its associated call is expanded.
                    assert!(old_tag.is_none());
                    body.push((tok.0, Token::MacroTag(Some(tag)), tok.2))
                }

                _ => body.push(tok.clone()),
            }
        }

        if errs.is_empty() {
            Ok((body, macro_decl.sig_span.clone()))
        } else {
            Err(errs)
        }
    }
}

fn match_macro<'a>(
    macro_decls: &'a [MacroDecl],
    call: &MacroCall,
) -> Result<&'a MacroDecl, Vec<Error>> {
    // Firstly find the macros which have the correct name.
    let named_macros = macro_decls
        .iter()
        .filter(|md| md.name.name == call.name)
        .collect::<Vec<_>>();
    if named_macros.is_empty() {
        return Err(vec![Error::Compile {
            error: CompileError::MacroNotFound {
                name: call.name.clone(),
                span: call.span.clone(),
            },
        }]);
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
            vec![Error::Compile {
                error: CompileError::MacroCallMismatch {
                    name: call.name.clone(),
                    span: call.span.clone(),
                },
            }]
        })
}
