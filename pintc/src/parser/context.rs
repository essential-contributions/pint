use crate::{
    error::{CompileError, Error, Handler, ParseError},
    expr::{Expr, Immediate, TupleAccess},
    macros::{MacroCall, MacroDecl},
    parser::{Ident, NextModPath, UsePath, UseTree},
    predicate::{
        CallKey, ExprKey, Interface, InterfaceDecl, Predicate, PredicateInstance, Program,
        StorageVar, Var,
    },
    span::{self, Span, Spanned},
    types::{Path, PrimitiveKind, Type},
};
use std::collections::BTreeMap;

pub struct ParserContext<'a> {
    pub(crate) mod_path: &'a [String],
    pub(crate) mod_prefix: &'a str,
    pub(crate) local_scope: Option<&'a str>,
    pub(crate) program: &'a mut Program,
    pub(crate) current_pred: &'a mut String,
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
        if let Ok(name) = self.current_pred().add_top_level_symbol(
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

    /// Returns a mutable reference to the Pred named `self.current_pred`. Panics if the Pred cannot be
    /// found, indicating a bug.
    pub fn current_pred(&mut self) -> &mut Predicate {
        self.program.preds.get_mut(self.current_pred).unwrap()
    }

    /// Given an identifier and an type, produce a `StorageVar`. `l` and `r` are the code locations
    /// before and after the storage var declaration
    pub fn parse_storage_var(
        &self,
        handler: &Handler,
        name: Ident,
        ty: Type,
        (l, r): (usize, usize),
    ) -> StorageVar {
        let span = (self.span_from)(l, r);
        if ty.is_bool() || ty.is_int() || ty.is_b256() || ty.is_tuple() || ty.is_array() || ty.is_custom() {
            StorageVar { name, ty, span }
        } else if let Type::Map {
            ref ty_from,
            ref ty_to,
            ..
        } = ty
        {
            if (ty_from.is_bool() || ty_from.is_int() || ty_from.is_b256())
                && (ty_to.is_bool()
                    || ty_to.is_int()
                    || ty_to.is_b256()
                    || ty_to.is_map()
                    || ty_to.is_tuple()
                    || ty_to.is_array())
            {
                StorageVar { name, ty, span }
            } else {
                // TODO: allow arbitrary types in storage maps
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "currently in storage maps, keys must be int, bool, or b256 /
                              and values must be int and bool",
                        span: span::empty_span(),
                    },
                });
                StorageVar {
                    name,
                    ty: Type::Error(ty.span().clone()),
                    span,
                }
            }
        } 
        else {
            println!("ty: {:?}", &ty);
            // @mohammad, please review and advise
            // TODO: Fix here -- allow custom types that are maps, ints, or bools
            // - this is only for parsing, will need to be handled later
            // - means we can't check if it will be lowered to a map or not, so just need to allow it
            // which I did above in the first if conditional
            // - can handle during analyse.rs?
            // TODO: allow arbitrary types in storage blocks
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "only ints, bools, custom types, and maps are currently allowed in a storage block",
                    span: span::empty_span(),
                },
            });
            StorageVar {
                name,
                ty: Type::Error(ty.span().clone()),
                span,
            }
        }
    }

    /// Given a list of storage variables, check that there are no duplicate names and return the
    /// same list
    pub fn parse_storage_block(
        &mut self,
        handler: &Handler,
        storage_vars: Vec<StorageVar>,
    ) -> Vec<StorageVar> {
        let mut storage_symbols: BTreeMap<String, Span> = BTreeMap::new();
        for var in &storage_vars {
            if let Some(prev_span) = storage_symbols.get(&var.name.name) {
                handler.emit_err(Error::Parse {
                    error: ParseError::NameClash {
                        sym: var.name.name.clone(),
                        span: var.name.span.clone(),
                        prev_span: prev_span.clone(),
                    },
                });
            } else {
                storage_symbols.insert(var.name.name.clone(), var.name.span.clone());
            }
        }

        storage_vars
    }

    /// Given a list of storage variables insert it into the current Pred after some error checking.
    /// `l` and `r` are the code locations before and after the storage block declaration
    pub fn parse_storage_decl(
        &mut self,
        handler: &Handler,
        storage_vars: Vec<StorageVar>,
        (l, r): (usize, usize),
    ) {
        let span = (self.span_from)(l, r);
        if let Some((_, prev_span)) = &self.current_pred().storage {
            // Multiple `storage` blocks are not allowed
            handler.emit_err(Error::Parse {
                error: ParseError::TooManyStorageBlocks {
                    span,
                    prev_span: prev_span.clone(),
                },
            });
        } else if !self.mod_path.is_empty() {
            // `storage` blocks in sub-modules are not allowed
            handler.emit_err(Error::Parse {
                error: ParseError::StorageDirectiveMustBeTopLevel { span },
            });
        } else {
            self.current_pred().storage = Some((storage_vars, span));
        }
    }

    /// Given an interface name as an `Ident` and a list of `InterfaceDecl`s (which can be storage
    /// blocks or predicate interfaces), produce an `Interface` object and insert it into the current
    /// Pred. `l` and `r` are the code locations before and after the interface declaration
    pub fn parse_interface(
        &mut self,
        handler: &Handler,
        name: Ident,
        interface_decls: Vec<InterfaceDecl>,
        (l, r): (usize, usize),
    ) {
        let mut interface = Interface {
            name: self.add_top_level_symbol(handler, name, self.mod_prefix),
            storage: None,
            predicate_interfaces: vec![],
            span: (self.span_from)(l, r),
        };

        let mut predicate_names: BTreeMap<String, Span> = BTreeMap::new();
        for decl in interface_decls {
            match decl {
                InterfaceDecl::StorageDecl(d) => {
                    if let Some((_, prev_span)) = &interface.storage {
                        // Multiple `storage` blocks are not allowed
                        handler.emit_err(Error::Parse {
                            error: ParseError::TooManyStorageBlocks {
                                span: d.1,
                                prev_span: prev_span.clone(),
                            },
                        });
                    } else {
                        interface.storage = Some(d)
                    }
                }
                InterfaceDecl::PredicateInterface(predicate_interface) => {
                    // Ensure there are no duplciate predicate names
                    if let Some(prev_span) = predicate_names.get(&predicate_interface.name.name) {
                        handler.emit_err(Error::Parse {
                            error: ParseError::NameClash {
                                sym: predicate_interface.name.name.clone(),
                                span: predicate_interface.name.span.clone(),
                                prev_span: prev_span.clone(),
                            },
                        });
                    } else {
                        predicate_names.insert(
                            predicate_interface.name.name.clone(),
                            predicate_interface.name.span.clone(),
                        );
                    }

                    // Ensure there are no duplciate vars
                    let mut var_symbols: BTreeMap<String, Span> = BTreeMap::new();
                    for var in &predicate_interface.vars {
                        if let Some(prev_span) = var_symbols.get(&var.name.name) {
                            handler.emit_err(Error::Parse {
                                error: ParseError::NameClash {
                                    sym: var.name.name.clone(),
                                    span: var.name.span.clone(),
                                    prev_span: prev_span.clone(),
                                },
                            });
                        } else {
                            var_symbols.insert(var.name.name.clone(), var.name.span.clone());
                        }
                    }

                    interface.predicate_interfaces.push(predicate_interface)
                }
            }
        }

        self.current_pred().interfaces.push(interface);
    }

    /// Given a predicate instance name as an `Predicate`, a list `els` of `Ident`s forming a path, an
    /// predicate name as an `Predicate` and an address as an `ExprKey`, produce an `PredicateInstance`
    /// object and insert it into the current Pred. `l` and `r` are the code locations before and
    /// after the interface declaration. `l1` and `r1` are the code locations before and after the
    /// path represented by `els`. Uses `is_abs` to decide how to handle the path `els`.
    #[allow(clippy::too_many_arguments)]
    pub fn parse_predicate_instance(
        &mut self,
        handler: &Handler,
        name: Ident,
        is_abs: bool,
        els: Vec<Ident>,
        predicate: Ident,
        address: ExprKey,
        (l, l1, r1, r): (usize, usize, usize, usize),
    ) {
        let interface_instance = if els.is_empty() {
            handler.emit_err(Error::Parse {
                error: ParseError::PathTooShort {
                    path: if is_abs {
                        "::".to_owned() + &predicate.name
                    } else {
                        predicate.name.clone()
                    },
                    span: (self.span_from)(l, r),
                },
            });
            "".to_string()
        } else {
            let els_len = els.len();
            let path_span = (self.span_from)(l1, r1);
            if is_abs {
                self.parse_absolute_path(
                    els[..els_len - 1].to_vec(),
                    els[els_len - 1].clone(),
                    false,
                    path_span,
                )
            } else {
                self.parse_relative_path(
                    els[..els_len - 1].to_vec(),
                    els[els_len - 1].clone(),
                    false,
                    path_span,
                )
            }
        };

        let span = (self.span_from)(l, r);
        let full_predicate_instance_name =
            self.add_top_level_symbol(handler, name.clone(), self.mod_prefix);
        let predicate_instance = PredicateInstance {
            name: full_predicate_instance_name.clone(),
            interface_instance,
            predicate,
            address,
            span: span.clone(),
        };
        self.current_pred()
            .predicate_instances
            .push(predicate_instance);

        // Also insert a local integer decision variable that represents the pathway corresponding
        // to this particular predicate instance
        self.current_pred().vars.insert(
            Var {
                name: "__".to_string() + &full_predicate_instance_name.to_string() + "_pathway",
                is_pub: false,
                span,
            },
            Type::Primitive {
                kind: PrimitiveKind::Int,
                span: span::empty_span(),
            },
        );
    }

    /// Given an identifier an optional type, and an optional initializer `ExprKey`, produce a
    /// `Var` and insert it into the current Pred. `l` and `r` are the code locations before and
    /// after the var declaration. `is_pub` determines the visibility of the `Var`.
    pub fn parse_var_decl(
        &mut self,
        handler: &Handler,
        is_pub: bool,
        name: (Ident, Option<&'a str>),
        ty: Option<Type>,
        init: Option<ExprKey>,
        (l, r): (usize, usize),
    ) {
        if ty.is_none() && init.is_none() {
            handler.emit_err(Error::Parse {
                error: ParseError::UntypedVariable {
                    name: name.0.name,
                    span: (self.span_from)(l, r),
                },
            });
        } else {
            let mod_prefix = self.mod_prefix;
            let _ = self
                .current_pred()
                .insert_var(handler, mod_prefix, name.1, is_pub, &name.0, ty)
                .map(|var_key| {
                    if let Some(expr_key) = init {
                        self.current_pred().var_inits.insert(var_key, expr_key);
                        let span = (self.span_from)(l, r);
                        self.current_pred()
                            .insert_eq_or_ineq_constraint(var_key, expr_key, span);
                    }
                });
        }
    }

    /// Given an identifier (a string + a bool indicating whethere it's in a macro argument),
    /// produce an `Ident` and an optional string that contains the current local scope, if needed
    /// (useful for macros). `l` and `r` are the code locations before and after the identifier
    pub fn parse_var_name(
        &mut self,
        handler: &Handler,
        id: (String, bool),
        (l, r): (usize, usize),
    ) -> (Ident, Option<&'a str>) {
        let name = id.0.to_string();
        let span = (self.span_from)(l, r);
        if name.starts_with("__") {
            handler.emit_err(Error::Parse {
                error: ParseError::LeadingUnderscoresInIdent {
                    span: span.clone(),
                    name: name.clone(),
                },
            });
        }

        // We special case the let name here, as we're interested in the associated flag (and this
        // is the only place where we care).  The flag indicates that this identifier was
        // substituted as a part of a macro argument during macro expansion.
        //
        // This is to implement macro body hygiene.
        //
        // If we have an identifier which is NOT from a macro arg AND a local prefix set in the
        // parser context indicating we're currently parsing a macro body, then we need to add a
        // Use path to make sure this name is referred to using that local prefix.  We're adding
        // hygiene using the prefix.

        let local_scope = (!id.1).then_some(()).and(self.local_scope);

        if let Some(prefix) = local_scope {
            let mut path = self.mod_path.to_vec();
            path.push(prefix.to_owned());
            path.push(id.0.to_owned());
            self.use_paths.push(UsePath {
                path,
                alias: None,
                is_absolute: true,
                span: span::empty_span(),
            });
        }

        let name = Ident {
            name: name.clone(),
            hygienic: id.1,
            span,
        };

        (name, local_scope)
    }

    /// Given a list of `Ident`s and a last `Ident` that represent a *absolute* path, produce a
    /// `Path` that represents that path and append an appropriate `NextModPath` to trigger a
    /// compilation of the module containing the path.
    pub fn parse_absolute_path(
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
    pub fn parse_relative_path(
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

    /// Given a `UseTree`, produce a list of use paths and append them to `self.use_paths`.
    /// `is_abs` determines whether the path is absolute or not. `l` and `r` are the code locations
    /// before and after the use statement.
    pub fn parse_use_statement(
        &mut self,
        handler: &Handler,
        is_abs: bool,
        use_tree: UseTree,
        (l, r): (usize, usize),
    ) {
        // Convert the use tree into use paths, prepend the current mod path prefix to each iff the
        // use tree is not absolute, and append to the current list of use paths in our context.
        let mod_prefix = self.mod_prefix;
        let mod_path = self.mod_path;
        let local_handler = Handler::default();
        let mut new_use_paths = use_tree
            .gather_paths()
            .into_iter()
            .filter_map(|mut use_path| {
                if !is_abs {
                    use_path.add_prefix(mod_path.to_vec());
                }

                // If any of the idents in the path, other than the last one, is a `self`,
                // immediately error out
                if use_path
                    .path
                    .iter()
                    .take(use_path.path.len() - 1)
                    .any(|elem| elem == "self")
                {
                    local_handler.emit_err(Error::Parse {
                        error: ParseError::SelfNotAtTheEnd {
                            // We can use a better span here but that's okay for now. Ideally, we
                            // would use the span of the `self` ident itself, but we don't have that
                            // right now.
                            span: (self.span_from)(l, r),
                        },
                    });
                    return None;
                }

                // Paths that end in `self` get a special handling
                if use_path.path[use_path.path.len() - 1] == "self" {
                    // First, remove `self` because a path that ends in `self` is the same as its
                    // prefix For example: `a::b::self` is the same as `a::b`
                    use_path.path.pop();

                    // Paths with only `self` (i.e. nothing before it) are not valid
                    //
                    // Check that the prefix `use_path.path` matches the current `mod_path` because
                    // we may in a module other than the root module
                    if use_path.path == mod_path {
                        local_handler.emit_err(Error::Parse {
                            error: ParseError::SelfWithEmptyPrefix {
                                span: use_path.span.clone(),
                            },
                        });
                        return None;
                    }
                }

                Some(use_path.clone())
            })
            .map(|use_path| {
                // This is a bit strange.  We're taking the alias or the last element of the use
                // path, prefixing it with the current mod path and inserting that as a top-level
                // symbol.  This is to avoid local decls clashing with the `use` path, though those
                // symbols are all absolute.
                //
                // e.g.,
                // use some::other::mod::a;       // Inserted as ::local::mod::a
                // let a: int;                    // Inserted as ::local::mod::a
                //
                // use some::other::mod::b as c;  // Inserted as ::local::mod::c
                // let c: int;                    // Inserted as ::local::mod::c
                //
                // There is one special case with `self` where the previous ident in the path is
                // considered instead. This is automatically handled by the `filter_map` above. For
                // example:
                //
                // use a::b::mod::my_mod::self;    // Inserted as ::local::mod::my_mod
                self.current_pred()
                    .add_top_level_symbol(
                        &local_handler,
                        mod_prefix,
                        None,
                        &Ident {
                            name: use_path
                                .alias
                                .clone()
                                .unwrap_or(use_path.path[use_path.path.len() - 1].clone()),
                            hygienic: false,
                            span: use_path.span.clone(),
                        },
                        use_path.span.clone(),
                    )
                    .map(|_| use_path.clone())
                    .unwrap_or_else(|_| use_path)
            })
            .collect::<Vec<_>>();

        handler.append(local_handler);
        self.use_paths.append(&mut new_use_paths);
    }

    /// Parses a tuple access expression with an identifier.
    ///
    /// Given an `ExprKey` and an `Ident`, insert a new `TupleFieldAccess` expression into
    /// `current_pred().exprs`. This function also takes two integers `l` and `r`:
    /// - `l` is the source code location before the tuple access
    /// - `r` is the source code location after the tuple access
    pub fn parse_tuple_field_op_with_ident(
        &mut self,
        tuple: ExprKey,
        name: Ident,
        (l, r): (usize, usize),
    ) -> ExprKey {
        let span = (self.span_from)(l, r);
        self.current_pred().exprs.insert(
            Expr::TupleFieldAccess {
                tuple,
                field: TupleAccess::Name(name),
                span: span.clone(),
            },
            Type::Unknown(span),
        )
    }

    /// Parses a tuple access expression with an integer.
    ///
    /// Given an `ExprKey` and a string that represents an integer, insert a new `TupleFieldAccess`
    /// expression into `current_pred().exprs`. This function also takes three integers `l`, `m`, and
    /// `r`:
    /// - `l` is the source code location before the tuple access
    /// - `m` is the source code location before the integer used to access the tuple
    /// - `r` is the source code location after the tuple access
    pub fn parse_tuple_field_op_with_int(
        &mut self,
        handler: &Handler,
        tuple: ExprKey,
        int_str: String,
        (l, m, r): (usize, usize, usize),
    ) -> ExprKey {
        let span = (self.span_from)(l, r);
        let index_span = (self.span_from)(m, r);
        let int_str = int_str.replace('_', "");

        self.current_pred().exprs.insert(
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

    /// Parses a tuple access expression with a real (e.g. `my_tuple.1.3 - the `1.3` here is a
    /// real).
    ///
    /// Given an `ExprKey` and a string that represents a real, insert a new (nested)
    /// `TupleFieldAccess` expression into `current_pred().exprs`. This function also takes three
    /// integers `l`, `m`, and `r`:
    /// - `l` is the source code location before the tuple access
    /// - `m` is the source code location before the real used to access the tuple
    /// - `r` is the source code location after the tuple access
    pub fn parse_tuple_field_op_with_real(
        &mut self,
        handler: &Handler,
        tuple: ExprKey,
        real_str: String,
        (l, m, r): (usize, usize, usize),
    ) -> ExprKey {
        let real_str = real_str.replace('_', "");
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
                let lhs_access_key = self.current_pred().exprs.insert(
                    Expr::TupleFieldAccess {
                        tuple,
                        field: first_index,
                        span: span.clone(),
                    },
                    Type::Unknown(span),
                );

                let span = (self.span_from)(l, r);
                self.current_pred().exprs.insert(
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
                self.current_pred().exprs.insert(
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

    /// Given a String that represents an integer literal, produce an `Immediate`. The integer can
    /// be in any form (decimal, hex, or binary). `l` and `r` are the code locations before and
    /// after the integer literal.
    pub fn parse_int_immediate(
        &self,
        handler: &Handler,
        s: String,
        (l, r): (usize, usize),
    ) -> Immediate {
        let span = (self.span_from)(l, r);
        let s = s.replace('_', "");
        match s.chars().nth(1) {
            Some('b') => {
                let digits = s.len() - 2;
                match digits {
                    1..=63 => Immediate::Int(i64::from_str_radix(&s[2..], 2).unwrap()),
                    64 => {
                        // Full 64 bits are specified: check the MSB which represents the sign bit,
                        // and parse accordingly.
                        if &s[2..3] == "1" {
                            // Negative integer. Parse as `u64` then convert to `i64`.
                            Immediate::Int(u64::from_str_radix(&s[2..], 2).unwrap() as i64)
                        } else {
                            // Positive integer. Parse as `i64` directly.
                            Immediate::Int(i64::from_str_radix(&s[2..], 2).unwrap())
                        }
                    }
                    256 => {
                        let offset = 2;
                        Immediate::B256([
                            u64::from_str_radix(&s[offset..offset + 64], 2).unwrap(),
                            u64::from_str_radix(&s[offset + 64..offset + 128], 2).unwrap(),
                            u64::from_str_radix(&s[offset + 128..offset + 192], 2).unwrap(),
                            u64::from_str_radix(&s[offset + 192..offset + 256], 2).unwrap(),
                        ])
                    }
                    _ => {
                        handler.emit_err(Error::Parse {
                            error: ParseError::BinaryLiteralLength {
                                digits,
                                span: span.clone(),
                            },
                        });
                        Immediate::Error
                    }
                }
            }
            Some('x') => {
                let digits = s.len() - 2;
                match digits {
                    1..=15 => Immediate::Int(i64::from_str_radix(&s[2..], 16).unwrap()),
                    16 => {
                        // Full 16 hex digits are specified: check the MSB which represents the sign
                        // bit, and parse accordingly.
                        match &s[2..3] {
                            // Negative integer. Parse as `u64` then convert to `i64`.
                            "8" | "9" | "a" | "A" | "b" | "B" | "c" | "C" | "d" | "D" | "e"
                            | "E" | "f" | "F" => {
                                Immediate::Int(u64::from_str_radix(&s[2..], 16).unwrap() as i64)
                            }
                            // Positive integer. Parse as `i64` directly.
                            _ => Immediate::Int(i64::from_str_radix(&s[2..], 16).unwrap()),
                        }
                    }
                    64 => {
                        let offset = 2;
                        Immediate::B256([
                            u64::from_str_radix(&s[offset..offset + 16], 16).unwrap(),
                            u64::from_str_radix(&s[offset + 16..offset + 32], 16).unwrap(),
                            u64::from_str_radix(&s[offset + 32..offset + 48], 16).unwrap(),
                            u64::from_str_radix(&s[offset + 48..offset + 64], 16).unwrap(),
                        ])
                    }
                    _ => {
                        handler.emit_err(Error::Parse {
                            error: ParseError::HexLiteralLength {
                                digits,
                                span: span.clone(),
                            },
                        });
                        Immediate::Error
                    }
                }
            }
            _ => match s.parse::<i64>() {
                Ok(val) => Immediate::Int(val),
                Err(_) => {
                    handler.emit_err(Error::Parse {
                        error: ParseError::IntLiteralTooLarge { span: span.clone() },
                    });
                    Immediate::Error
                }
            },
        }
    }

    /// Given an optional path (a list of identifiers followed by a final one) and a var `name`,
    /// produce a `StorageAccess`. Uses `is_abs` to decide how to handle the path. `l` and `r` are
    /// the code locations before and after the storage access. `r` is the code location right
    /// after the path (before `::storage::` shows up).
    pub fn parse_storage_access(
        &mut self,
        handler: &Handler,
        is_abs: bool,
        path: Option<(Vec<Ident>, Ident)>,
        name: Ident,
        (l, m, r): (usize, usize, usize),
    ) -> ExprKey {
        if let Some((els, last)) = path {
            let path_span = (self.span_from)(l, m);
            let interface_instance = if is_abs {
                self.parse_absolute_path(els, last, false, path_span)
            } else {
                self.parse_relative_path(els, last, false, path_span)
            };

            let span = (self.span_from)(l, r);
            self.current_pred().exprs.insert(
                Expr::ExternalStorageAccess {
                    interface_instance,
                    name: name.to_string(),
                    span: span.clone(),
                },
                Type::Unknown(span),
            )
        } else {
            let span = (self.span_from)(l, r);
            let expr = if !self.mod_path.is_empty() {
                // `storage` blocks in sub-modules are not allowed
                handler.emit_err(Error::Parse {
                    error: ParseError::StorageAccessMustBeTopLevel { span: span.clone() },
                });
                Expr::Error(span.clone())
            } else {
                Expr::StorageAccess(name.to_string(), span.clone())
            };
            self.current_pred().exprs.insert(expr, Type::Unknown(span))
        }
    }
}
