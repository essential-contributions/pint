use crate::{
    error::{Error, Handler, ParseError},
    expr::{Expr, TupleAccess},
    intermediate::{CallKey, ExprKey, IntermediateIntent, Program},
    macros::{MacroCall, MacroDecl},
    parser::{Ident, NextModPath, UsePath},
    span::Span,
    types::{Path, Type},
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

    /// Handles a tuple access expression with an identifier.
    ///
    /// Given an `ExprKey` and an `Ident`, insert a new `TupleFieldAccess` expression into
    /// `current_ii().exprs`. This function also takes two integers `l` and `r`:
    /// - `l` is the source code location before the tuple access
    /// - `r` is the source code location after the tuple access
    pub fn handle_tuple_field_op_with_ident(
        &mut self,
        tuple: ExprKey,
        name: Ident,
        (l, r): (usize, usize),
    ) -> ExprKey {
        let span = (self.span_from)(l, r);
        self.current_ii().exprs.insert(
            Expr::TupleFieldAccess {
                tuple,
                field: TupleAccess::Name(name),
                span: span.clone(),
            },
            Type::Unknown(span),
        )
    }

    /// Handles a tuple access expression with an integer.
    ///
    /// Given an `ExprKey` and a string that represents an integer, insert a new `TupleFieldAccess`
    /// expression into `current_ii().exprs`. This function also takes three integers `l`, `m`, and
    /// `r`:
    /// - `l` is the source code location before the tuple access
    /// - `m` is the source code location before the integer used to access the tuple
    /// - `r` is the source code location after the tuple access
    pub fn handle_tuple_field_op_with_int(
        &mut self,
        handler: &Handler,
        tuple: ExprKey,
        int_str: String,
        (l, m, r): (usize, usize, usize),
    ) -> ExprKey {
        let span = (self.span_from)(l, r);
        let index_span = (self.span_from)(m, r);

        self.current_ii().exprs.insert(
            Expr::TupleFieldAccess {
                tuple,
                field: int_str
                    .parse::<usize>()
                    .map(TupleAccess::Index)
                    .unwrap_or_else(|_| {
                        // Recover with a malformed field access
                        handler.emit_err(Error::Parse {
                            error: ParseError::InvalidIntegerTupleIndex {
                                span: index_span,
                                index: int_str,
                            },
                        });
                        TupleAccess::Error
                    }),
                span: span.clone(),
            },
            Type::Unknown(span),
        )
    }

    /// Handles a tuple access expression with a real (e.g. `my_tuple.1.3 - the `1.3` here is a
    /// real).
    ///
    /// Given an `ExprKey` and a string that represents a real, insert a new (nested)
    /// `TupleFieldAccess` expression into `current_ii().exprs`. This function also takes three
    /// integers `l`, `m`, and `r`:
    /// - `l` is the source code location before the tuple access
    /// - `m` is the source code location before the real used to access the tuple
    /// - `r` is the source code location after the tuple access
    pub fn handle_tuple_field_op_with_real(
        &mut self,
        handler: &Handler,
        tuple: ExprKey,
        real_str: String,
        (l, m, r): (usize, usize, usize),
    ) -> ExprKey {
        match real_str.chars().position(|c| c == '.') {
            Some(dot_index) => {
                let first_index = real_str[0..dot_index]
                    .parse::<usize>()
                    .map(TupleAccess::Index)
                    .unwrap_or_else(|_| {
                        handler.emit_err(Error::Parse {
                            error: ParseError::InvalidIntegerTupleIndex {
                                span: (self.span_from)(m, m + dot_index),
                                index: real_str[0..dot_index].to_string(),
                            },
                        });

                        // Recover with a malformed tuple access
                        TupleAccess::Error
                    });

                let second_index = real_str[(dot_index + 1)..]
                    .parse::<usize>()
                    .map(TupleAccess::Index)
                    .unwrap_or_else(|_| {
                        handler.emit_err(Error::Parse {
                            error: ParseError::InvalidIntegerTupleIndex {
                                span: (self.span_from)(m + dot_index + 1, r),
                                index: real_str[(dot_index + 1)..].to_string(),
                            },
                        });

                        // Recover with a malformed tuple access
                        TupleAccess::Error
                    });

                let span = (self.span_from)(l, m + dot_index);
                let lhs_access_key = self.current_ii().exprs.insert(
                    Expr::TupleFieldAccess {
                        tuple,
                        field: first_index,
                        span: span.clone(),
                    },
                    Type::Unknown(span),
                );

                let span = (self.span_from)(l, r);
                self.current_ii().exprs.insert(
                    Expr::TupleFieldAccess {
                        tuple: lhs_access_key,
                        field: second_index,
                        span: span.clone(),
                    },
                    Type::Unknown(span),
                )
            }
            None => {
                handler.emit_err(Error::Parse {
                    error: ParseError::InvalidTupleIndex {
                        span: (self.span_from)(m, r),
                        index: real_str.to_string(),
                    },
                });

                // Recover with a malformed tuple access
                let span = (self.span_from)(l, r);
                self.current_ii().exprs.insert(
                    Expr::TupleFieldAccess {
                        tuple,
                        field: TupleAccess::Error,
                        span: span.clone(),
                    },
                    Type::Unknown(span),
                )
            }
        }
    }
    /// Given a list of `Ident`s and a last `Ident` that represent a *absolute* path, produce a
    /// `Path` that represents that path and append an appropriate `NextModPath` to trigger a
    /// compilation of the module containing the path.
    pub fn handle_absolute_path(
        &mut self,
        els: Vec<Ident>,
        last: Ident,
        maybe_enum: bool,
        span: Span,
    ) -> Path {
        if !els.is_empty() {
            let path: Vec<_> = els.iter().map(|el| el.to_string()).collect();
            self.next_paths.push(NextModPath {
                is_abs: true,
                mod_path_strs: path.clone(),
                suffix: last.to_string(),
                enum_path_strs: if maybe_enum {
                    (path.len() > 1).then_some(path.iter().take(path.len() - 1).cloned().collect())
                } else {
                    None
                },
                span,
            });
        }

        format!(
            "::{}{last}",
            els.iter()
                .map(|el| format!("{el}::"))
                .collect::<Vec<_>>()
                .concat()
        )
    }

    /// Given a list of `Ident`s and a last `Ident` that represent a *relative* path, produce a
    /// `Path` that represents that path and append an appropriate `NextModPath` to trigger a
    /// compilation of the module containing the path. Take into account what `use` statements this
    /// modules has to figure out if the path is already imported.
    pub fn handle_relative_path(
        &mut self,
        els: Vec<Ident>,
        last: Ident,
        maybe_enum: bool,
        span: Span,
    ) -> Path {
        // Check if any of the use statement matches the path. This requires
        // that the alias (if it exists) or the last ident in the use statement
        // matches the first ident in the path.
        // For example:
        // - `use a::b` and `b::c` match.
        // - `use a::b as d` and `d::e` match.
        let path_prefix = els
            .first()
            .map(|id| id.to_string())
            .unwrap_or_else(|| last.to_string());
        let full_absolute_path = self
            .use_paths
            .iter()
            .find(|use_path| use_path.matches_suffix(&path_prefix))
            .and_then(|use_path| {
                // We've found a use path which matches.  Construct a full path by joining the it
                // with the parsed path. `parsed_path_iter` is our parsed path except for the first
                // element.
                let parsed_path_iter = els
                    .iter()
                    .chain(std::iter::once(&last))
                    .skip(1)
                    .map(|el| el.to_string());

                let mut full_path = use_path
                    .path
                    .iter()
                    .cloned()
                    .chain(parsed_path_iter)
                    .collect::<Vec<_>>();

                let full_path_str = full_path
                    .iter()
                    .map(|el| format!("::{el}"))
                    .collect::<Vec<_>>()
                    .concat();

                // The next paths don't include the final element, only paths to modules or enums.
                full_path.pop();
                self.next_paths.push(NextModPath {
                    is_abs: true,
                    mod_path_strs: full_path.clone(),
                    suffix: last.to_string(),
                    enum_path_strs: if maybe_enum {
                        (full_path.len() > 1).then_some(
                            full_path
                                .iter()
                                .take(full_path.len() - 1)
                                .cloned()
                                .collect(),
                        )
                    } else {
                        None
                    },
                    span: span.clone(),
                });

                if last.hygienic {
                    // This identifier is hygienic and should not have 'use' paths prepended
                    // afterall.
                    None
                } else {
                    Some(full_path_str)
                }
            })
            .unwrap_or_else(|| {
                // We didn't find a matching use path.  Just return the parsed path as is.
                if !els.is_empty() {
                    let path: Vec<_> = els.iter().map(|el| el.to_string()).collect();
                    self.next_paths.push(NextModPath {
                        is_abs: false,
                        mod_path_strs: path.clone(),
                        suffix: last.to_string(),
                        enum_path_strs: if maybe_enum {
                            (path.len() > 1)
                                .then_some(path.iter().take(path.len() - 1).cloned().collect())
                        } else {
                            None
                        },
                        span: span.clone(),
                    });
                }
                format!(
                    "{}{}{last}",
                    self.mod_prefix,
                    els.iter()
                        .map(|el| format!("{el}::"))
                        .collect::<Vec<_>>()
                        .concat()
                )
            });

        full_absolute_path
    }
}
