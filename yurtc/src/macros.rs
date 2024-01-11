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

pub(crate) fn expand_call(
    macro_decls: &[MacroDecl],
    call: &MacroCall,
) -> Result<Vec<(usize, Token, usize)>, Vec<Error>> {
    let macro_decl: &MacroDecl = match_macro(macro_decls, call)?;
    let mut body = Vec::with_capacity(macro_decl.body.len());
    let mut errs = Vec::new();

    let param_idcs = HashMap::<&String, usize>::from_iter(
        macro_decl
            .params
            .iter()
            .enumerate()
            .map(|(idx, id)| (&id.name, idx)),
    );

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
            _ => body.push(tok.clone()),
        }
    }

    if errs.is_empty() {
        Ok(body)
    } else {
        Err(errs)
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
