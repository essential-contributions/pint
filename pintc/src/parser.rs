use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler, ParseError},
    expr::{Expr, Ident},
    lexer,
    macros::{self, MacroCall, MacroDecl, MacroExpander},
    predicate::{CallKey, ExprKey, Exprs, Interface, Predicate, Program},
    span::{empty_span, Span, Spanned},
    types::*,
};

use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
    rc::Rc,
};

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(#[allow(clippy::ptr_arg, clippy::type_complexity)] pub pint_parser);

mod use_path;
pub(crate) use use_path::{UsePath, UseTree};

mod context;
pub(crate) use context::ParserContext;

#[cfg(test)]
mod tests;

pub fn parse_project(
    handler: &Handler,
    deps: &Dependencies,
    root_src_path: &Path,
) -> Result<Program, ErrorEmitted> {
    ProjectParser::new(handler, deps, PathBuf::from(root_src_path))
        .parse_project()
        .finalize()
}

struct ProjectParser<'a> {
    program: Program,
    macros: Vec<MacroDecl>,
    macro_calls: BTreeMap<String, slotmap::SecondaryMap<CallKey, (ExprKey, MacroCall)>>,
    proj_root_path: PathBuf,
    root_src_path: PathBuf,
    visited_paths: Vec<PathBuf>,
    handler: &'a Handler,
    deps: &'a Dependencies<'a>,
    unique_idx: u64,
}

/// External dependency names to their associated entry point.
///
/// Each dependency name is treated as a submodule.
type Dependencies<'a> = fxhash::FxHashMap<&'a str, &'a Path>;

#[derive(Clone, Debug)]
pub(crate) struct NextModPath {
    // Determines whether the path of the current module needs to be used as a prefix for next path
    pub(crate) is_abs: bool,

    // This is the module path to consider if the suffix refers to the name of a declaration
    pub(crate) mod_path_strs: Vec<String>,

    // This is the suffix, which may refer to the name of a declaration or an enum variant
    pub(crate) suffix: String,

    // This is the module path to consider if the suffix refers to an enum variant
    pub(crate) enum_path_strs: Option<Vec<String>>,

    // This is the span of the original full path (including the suffix)
    pub(crate) span: Span,
}

/// Part of the result of `find_next_path`.
enum FoundPath {
    /// The file path is some dependency.
    Dep,
    /// The file path is local to this project.
    Local,
}

impl<'a> ProjectParser<'a> {
    fn new(handler: &'a Handler, deps: &'a Dependencies<'a>, root_src_path: PathBuf) -> Self {
        let proj_root_path = root_src_path
            .parent()
            .map_or_else(|| PathBuf::from("/"), PathBuf::from);

        let mut project_parser = Self {
            program: Program::default(),
            macros: vec![],
            macro_calls: BTreeMap::from([(
                Program::ROOT_PRED_NAME.to_string(),
                slotmap::SecondaryMap::default(),
            )]),
            proj_root_path,
            root_src_path,
            visited_paths: vec![],
            handler,
            deps,
            unique_idx: 0,
        };

        // Start with an empty Pred with an empty name. This is the "root predicate".
        project_parser
            .program
            .preds
            .insert(Program::ROOT_PRED_NAME.to_string(), Predicate::default());

        project_parser
    }

    /// Parse the project starting with the `root_src_path` and return a predicate. Upon failure,
    /// return a vector of all compile errors encountered.
    fn parse_project(mut self) -> Self {
        let mut call_replacements = Vec::<(String, ExprKey, Option<ExprKey>, Span)>::new();
        let mut macro_expander = MacroExpander::default();
        let mut pending_paths = vec![(self.root_src_path.clone(), Vec::new())];

        // This is an unbound loop which breaks if there are errors or when there is no more work
        // to do, as per the pending_paths queue.  It will also break after a gazillion iterations
        // in case of an infinite loop bug.
        for loop_count in 0.. {
            while let Some((src_path, mod_path)) = pending_paths.pop() {
                if self.visited_paths.contains(&src_path) {
                    continue;
                }

                // Store this path as parsed to avoid re-parsing later.
                self.visited_paths.push(src_path.clone());

                // Parse this file module, returning any paths to other potential modules.
                let ((), next_paths) = self.parse_module(&Rc::from(src_path), &mod_path);
                self.analyse_and_add_paths(&mod_path, &next_paths, &mut pending_paths);
            }

            // Perform macro expansion for any new calls we have parsed. First, check for multiple
            // macros with the same signature.

            let some_non_unique = self
                .handler
                .scope(|handler| macros::verify_unique_set(handler, &self.macros))
                .is_err();

            // Abort here if we have non-unique macro declarations since call expansion assumes
            // that there will be a single match.
            if some_non_unique {
                return self;
            }

            let keys = self.macro_calls.keys().cloned().collect::<Vec<_>>();
            for current_pred in &keys {
                let macro_calls = self.macro_calls.get_mut(current_pred).unwrap();

                // Expand the next call. It may find new paths.
                if let Some(call_key) = macro_calls.keys().nth(0) {
                    let (call_expr_key, mut call) = macro_calls
                        .remove(call_key)
                        .expect("Call key must be valid.");

                    macros::splice_args(
                        self.handler,
                        self.program
                            .preds
                            .get(current_pred)
                            .expect("Call key must be valid"),
                        &mut call,
                    );

                    if let Ok((tokens, decl_sig_span)) = self
                        .handler
                        .scope(|handler| macro_expander.expand_call(handler, &self.macros, &call))
                    {
                        let (body_expr, next_paths) = self.parse_macro_body(
                            tokens,
                            &decl_sig_span.context,
                            &call.mod_path,
                            &call,
                            current_pred.clone(),
                        );

                        self.analyse_and_add_paths(&call.mod_path, &next_paths, &mut pending_paths);

                        call_replacements.push((
                            current_pred.clone(),
                            call_expr_key,
                            body_expr,
                            call.span.clone(),
                        ));
                    }
                }
            }

            for current_pred in keys {
                let macro_calls = self.macro_calls.get_mut(&current_pred).unwrap();
                if macro_calls.is_empty() {
                    self.macro_calls.remove(&current_pred);
                }
            }

            // If there's no more work then there's no more work.
            if pending_paths.is_empty() && self.macro_calls.iter().all(|(_, v)| v.is_empty()) {
                break;
            }

            // If the loop has gone for too long then there's an internal error. Arbitrary limit...
            if loop_count > 10_000 {
                self.handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "Infinite loop in project parser",
                        span: empty_span(),
                    },
                });
                return self;
            }
        }

        // For each call we need to replace its user (there should be just one) with the macro body
        // expression.  Or, for macros which only had declarations and no body expression, just
        // delete the call.
        for (current_pred, call_expr_key, body_expr_key, span) in call_replacements {
            let pred = self.program.preds.get_mut(&current_pred).unwrap();

            if let Some(body_expr_key) = body_expr_key {
                pred.replace_exprs(call_expr_key, body_expr_key);
            } else {
                // Keep track of the removed macro for type-checking, in case the predicate
                // erroneously expected the macro call to be an expression (and not just
                // declarations).
                pred.removed_macro_calls.insert(call_expr_key, span);
            }
            pred.exprs.remove(call_expr_key);
        }

        self
    }

    fn finalize(mut self) -> Result<Program, ErrorEmitted> {
        // Insert all enums, new types, and storage variables from the root Pred into each non-root
        // Pred (i.e. those declared using an `predicate { }` decl). Also, insert all top level
        // symbols since shadowing is not allowed. That is, we can't use a symbol inside an
        // `predicate { .. }` that was already used in the root Pred.

        macro_rules! process_nested_expr {
            ($expr_key: expr, $error_msg: literal, $root_exprs: expr, $pred: expr, $handler: expr) => {{
                let nested_expr = $root_exprs.get(*$expr_key).ok_or_else(|| {
                    $handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: concat!("missing ", $error_msg, " expr key in exprs slotmap"),
                            span: empty_span(),
                        },
                    })
                })?;
                $pred.exprs.insert(
                    nested_expr.clone(),
                    Type::Unknown(nested_expr.span().clone()),
                );
                deep_copy_expr(nested_expr, $root_exprs, $pred, $handler)
            }};
        }

        fn deep_copy_expr(
            expr: &Expr,
            root_exprs: &Exprs,
            pred: &mut Predicate,
            handler: &Handler,
        ) -> Result<(), ErrorEmitted> {
            match expr {
                Expr::Error(_)
                | Expr::Immediate { .. }
                | Expr::PathByKey(_, _)
                | Expr::PathByName(_, _)
                | Expr::StorageAccess(_, _)
                | Expr::ExternalStorageAccess { .. }
                | Expr::MacroCall { .. } => {}
                Expr::Array {
                    elements,
                    range_expr,
                    ..
                } => {
                    for element_expr_key in elements {
                        process_nested_expr!(
                            element_expr_key,
                            "array `element`",
                            root_exprs,
                            pred,
                            handler
                        )?;
                    }
                    process_nested_expr!(range_expr, "array `range`", root_exprs, pred, handler)?;
                }
                Expr::Tuple { fields, .. } => {
                    for (_, field_expr_key) in fields {
                        process_nested_expr!(
                            field_expr_key,
                            "tuple `field`",
                            root_exprs,
                            pred,
                            handler
                        )?;
                    }
                }
                Expr::UnaryOp { expr, .. } => {
                    process_nested_expr!(expr, "unary op", root_exprs, pred, handler)?;
                }
                Expr::BinaryOp { lhs, rhs, .. } => {
                    process_nested_expr!(lhs, "`lhs` of binary op", root_exprs, pred, handler)?;
                    process_nested_expr!(rhs, "`rhs` of binary op", root_exprs, pred, handler)?;
                }
                Expr::IntrinsicCall { args, .. } => {
                    for arg_expr_key in args {
                        process_nested_expr!(
                            arg_expr_key,
                            "intrinsic call `arg`",
                            root_exprs,
                            pred,
                            handler
                        )?;
                    }
                }
                Expr::Select {
                    condition,
                    then_expr,
                    else_expr,
                    ..
                } => {
                    process_nested_expr!(condition, "if `condition`", root_exprs, pred, handler)?;
                    process_nested_expr!(then_expr, "if `then expr`", root_exprs, pred, handler)?;
                    process_nested_expr!(else_expr, "if `else expr`", root_exprs, pred, handler)?;
                }
                Expr::Index { expr, index, .. } => {
                    process_nested_expr!(expr, "index `expr`", root_exprs, pred, handler)?;
                    process_nested_expr!(index, "index `index`", root_exprs, pred, handler)?;
                }
                Expr::TupleFieldAccess { tuple, .. } => {
                    process_nested_expr!(tuple, "tuple field access", root_exprs, pred, handler)?;
                }
                Expr::Cast { value, ty, .. } => {
                    process_nested_expr!(value, "cast `value`", root_exprs, pred, handler)?;
                    deep_copy_type(ty, root_exprs, pred, handler)?;
                }
                Expr::In {
                    value, collection, ..
                } => {
                    process_nested_expr!(value, "in `value`", root_exprs, pred, handler)?;
                    process_nested_expr!(collection, "in `collection`", root_exprs, pred, handler)?;
                }
                Expr::Range { lb, ub, .. } => {
                    process_nested_expr!(lb, "range `lower bound`", root_exprs, pred, handler)?;
                    process_nested_expr!(ub, "range `upper bound`", root_exprs, pred, handler)?;
                }
                Expr::Generator {
                    gen_ranges,
                    conditions,
                    body,
                    ..
                } => {
                    for (_, range_expr_key) in gen_ranges {
                        process_nested_expr!(
                            range_expr_key,
                            "generator `range`",
                            root_exprs,
                            pred,
                            handler
                        )?;
                    }
                    for condition_expr_key in conditions {
                        process_nested_expr!(
                            condition_expr_key,
                            "generator `condition`",
                            root_exprs,
                            pred,
                            handler
                        )?;
                    }
                    process_nested_expr!(body, "generator `body`", root_exprs, pred, handler)?;
                }
            }
            Ok(())
        }

        fn deep_copy_type(
            new_type: &Type,
            root_exprs: &Exprs,
            pred: &mut Predicate,
            handler: &Handler,
        ) -> Result<Type, ErrorEmitted> {
            match &new_type {
                Type::Array {
                    ty,
                    range,
                    size,
                    span,
                } => {
                    let range_expr = range
                        .and_then(|range| root_exprs.get(range))
                        .expect("exists");
                    deep_copy_expr(range_expr, root_exprs, pred, handler)?;
                    let new_expr_key = pred
                        .exprs
                        .insert(range_expr.clone(), Type::Unknown(range_expr.span().clone()));

                    Ok(Type::Array {
                        ty: Box::new(deep_copy_type(ty, root_exprs, pred, handler)?),
                        range: Some(new_expr_key),
                        size: *size,
                        span: span.clone(),
                    })
                }
                Type::Tuple { fields, span } => {
                    let mut new_fields: Vec<(Option<Ident>, Type)> = vec![];
                    for field in fields {
                        let new_field = (
                            field.0.clone(),
                            deep_copy_type(&field.1, root_exprs, pred, handler)?,
                        );
                        new_fields.push(new_field);
                    }
                    Ok(Type::Tuple {
                        fields: new_fields,
                        span: span.clone(),
                    })
                }
                Type::Alias { path, ty, span } => Ok(Type::Alias {
                    path: path.to_string(),
                    ty: Box::new(deep_copy_type(ty, root_exprs, pred, handler)?),
                    span: span.clone(),
                }),
                Type::Map {
                    ty_from,
                    ty_to,
                    span,
                } => Ok(Type::Map {
                    ty_from: Box::new(deep_copy_type(ty_from, root_exprs, pred, handler)?),
                    ty_to: Box::new(deep_copy_type(ty_to, root_exprs, pred, handler)?),
                    span: span.clone(),
                }),
                Type::Error(_)
                | Type::Unknown(_)
                | Type::Primitive { .. }
                | Type::Custom { .. } => Ok(new_type.clone()),
            }
        }

        fn deep_copy_new_types(
            root_new_types: &Vec<NewTypeDecl>,
            root_exprs: &Exprs,
            pred: &mut Predicate,
            handler: &Handler,
        ) -> Result<(), ErrorEmitted> {
            for new_type in root_new_types {
                let new_type_decl = deep_copy_type(&new_type.ty, root_exprs, pred, handler)?;
                pred.new_types.push(NewTypeDecl {
                    name: new_type.name.clone(),
                    ty: new_type_decl,
                    span: new_type.span.clone(),
                })
            }
            Ok(())
        }

        let enums = self.program.root_pred().enums.clone();
        let new_types = self.program.root_pred().new_types.clone();
        let root_symbols = self.program.root_pred().top_level_symbols.clone();
        let storage = self.program.root_pred().storage.clone();
        let interfaces = self.program.root_pred().interfaces.clone();
        let exprs = self.program.root_pred().exprs.clone();

        self.program
            .preds
            .iter_mut()
            .filter(|(name, _)| *name != &Program::ROOT_PRED_NAME.to_string())
            .for_each(|(_, pred)| {
                let _ = deep_copy_new_types(&new_types, &exprs, pred, self.handler);
                pred.enums.extend_from_slice(&enums);
                pred.storage = storage.as_ref().map(|storage| {
                    (
                        storage
                            .0
                            .iter()
                            .map(|storage_var| {
                                let mut new_storage_var = storage_var.clone();
                                new_storage_var.ty =
                                    deep_copy_type(&storage_var.ty, &exprs, pred, self.handler)
                                        .unwrap();
                                new_storage_var
                            })
                            .collect(),
                        storage.1.clone(),
                    )
                });
                pred.interfaces = interfaces
                    .iter()
                    .map(
                        |Interface {
                             name,
                             storage,
                             predicate_interfaces,
                             span,
                         }| Interface {
                            name: name.clone(),
                            storage: storage.as_ref().map(|storage| {
                                (
                                    storage
                                        .0
                                        .iter()
                                        .map(|storage_var| {
                                            let mut new_storage_var = storage_var.clone();
                                            new_storage_var.ty = deep_copy_type(
                                                &storage_var.ty,
                                                &exprs,
                                                pred,
                                                self.handler,
                                            )
                                            .unwrap();
                                            new_storage_var
                                        })
                                        .collect(),
                                    storage.1.clone(),
                                )
                            }),
                            // When we allow `pub var`s of type array, we should deep copy the
                            // types here too
                            predicate_interfaces: predicate_interfaces.clone(),
                            span: span.clone(),
                        },
                    )
                    .collect::<Vec<_>>();

                for (symbol, span) in &root_symbols {
                    // We could call `pred.add_top_level_symbol_with_name` directly here, but then
                    // the spans would be reversed so I decided to do this manually. We want the
                    // actual error to point to the symbol inside the `predicate` decl.
                    pred.top_level_symbols
                        .get(symbol)
                        .map(|prev_span| {
                            self.handler.emit_err(Error::Parse {
                                error: ParseError::NameClash {
                                    sym: symbol.clone(),
                                    span: prev_span.clone(),
                                    prev_span: span.clone(),
                                },
                            });
                        })
                        .unwrap_or_else(|| {
                            pred.top_level_symbols.insert(symbol.clone(), span.clone());
                        });
                }
            });

        if self.handler.has_errors() {
            return Err(self.handler.cancel());
        }

        Ok(self.program)
    }
}

// This is a macro for calling a LALRPOP parser with a full ParserContext.  It can't be a function
// because the parser needs to be passed in, and the parsers can't be generic types since they're
// different structs each time -- they don't implement a trait, they just all have identical
// `.parse()` methods.
//
// Perhaps it's confusing to put this in a macro, but if it's not then all of the context creation
// code here is duplicated.  *shrug*

macro_rules! parse_with {
    ($self: ident,
     $tokens: expr,
     $parser: expr,
     $src_path: ident,
     $mod_path: ident,
     $local_scope: expr,
     $macro_ctx: expr,
     $current_pred: expr,
     $handler: expr,
     ) => {{
        let span_from = |start, end| Span {
            context: Rc::clone($src_path),
            range: start..end,
        };

        let mut mod_prefix = $mod_path
            .iter()
            .map(|el| format!("::{el}"))
            .collect::<Vec<_>>()
            .concat();
        mod_prefix.push_str("::");
        let mut current_pred = $current_pred.to_string();

        let mut next_paths = Vec::new();

        let mut context = ParserContext {
            $mod_path,
            mod_prefix: &mod_prefix,
            local_scope: $local_scope,
            program: &mut $self.program,
            current_pred: &mut current_pred,
            macros: &mut $self.macros,
            macro_calls: &mut $self.macro_calls,
            span_from: &span_from,
            use_paths: &mut Vec::new(),
            next_paths: &mut next_paths,
        };

        let local_handler = Handler::default();

        let parsed_val = $parser
            .parse(&mut context, &local_handler, $tokens)
            .map_err(|lalrpop_err| {
                local_handler.emit_err(Error::Parse {
                    error: (lalrpop_err, $src_path).into(),
                });
            })
            .unwrap_or_default();

        if let Some((macro_name, macro_span)) = $macro_ctx {
            for err in local_handler.consume() {
                $handler.emit_err(Error::MacroBodyWrapper {
                    child: Box::new(err),
                    macro_name: macro_name.clone(),
                    macro_span: macro_span.clone(),
                });
            }
        } else {
            $self.handler.append(local_handler)
        }

        (parsed_val, next_paths)
    }};
}

impl<'a> ProjectParser<'a> {
    fn parse_module(&mut self, src_path: &Rc<Path>, mod_path: &[String]) -> ((), Vec<NextModPath>) {
        let src_str = fs::read_to_string(src_path).unwrap_or_else(|io_err| {
            self.handler.emit_err(Error::Compile {
                error: CompileError::FileIO {
                    error: io_err,
                    file: src_path.to_path_buf(),
                    span: empty_span(),
                },
            });
            String::new()
        });

        let mut mod_prefix = mod_path
            .iter()
            .map(|el| format!("::{el}"))
            .collect::<Vec<_>>()
            .concat();
        mod_prefix.push_str("");

        parse_with!(
            self,
            lexer::Lexer::new(&src_str, src_path, mod_path),
            pint_parser::PintParser::new(),
            src_path,
            mod_path,
            None,                           // local_scope
            Option::<(String, Span)>::None, // macro_ctx
            Program::ROOT_PRED_NAME, // The Pred when we explore a new module is always the root Pred
            self.handler,
        )
    }

    fn parse_macro_body(
        &mut self,
        tokens: Vec<(usize, lexer::Token, usize)>,
        src_path: &Rc<Path>,
        mod_path: &[String],
        macro_call: &MacroCall,
        current_pred: String,
    ) -> (Option<ExprKey>, Vec<NextModPath>) {
        let local_scope = format!("anon@{}", self.unique_idx);
        self.unique_idx += 1;
        parse_with!(
            self,
            lexer::Lexer::from_tokens(tokens, src_path, mod_path),
            pint_parser::MacroBodyParser::new(),
            src_path,
            mod_path,
            Some(&local_scope),
            Some((macro_call.name.clone(), macro_call.span.clone())),
            current_pred,
            self.handler,
        )
    }

    fn analyse_and_add_paths(
        &self,
        mod_path: &[String],
        next_paths: &[NextModPath],
        pending_paths: &mut Vec<(PathBuf, Vec<String>)>,
    ) {
        for NextModPath {
            is_abs,
            mod_path_strs,
            suffix,
            enum_path_strs,
            span,
        } in next_paths
        {
            // The idea here is to try to handle the next path assuming the suffix refers to a
            // declaration. If that fails, then try to handle the next path assuming it's a path to
            // an enum variant. If both options fail, then we emit an error.
            //
            // TODO: In the future if we want to support binary linking and
            // avoid re-parsing and re-type-checking dependencies, we probably
            // want to use `found` and only push to `pending_paths` in the case
            // that `FoundPath` is `FoundPath::Local`.
            if let Some((_found, next_path)) =
                self.find_next_path(*is_abs, mod_path_strs, mod_path, span)
            {
                pending_paths.push(next_path);
                continue;
            }

            if let Some(enum_path_strs) = enum_path_strs {
                if let Some((_found, next_path)) =
                    self.find_next_path(*is_abs, enum_path_strs, mod_path, span)
                {
                    pending_paths.push(next_path);
                    continue;
                }

                let path_mod = mod_path_strs.join("::");
                let path_enum = enum_path_strs.join("::");
                let path_full = format!("{path_mod}::{suffix}");

                self.handler.emit_err(Error::Compile {
                    error: CompileError::NoFileFoundForPath {
                        path_full: path_full.clone(),
                        path_mod,
                        path_enum,
                        span: span.clone(),
                    },
                });
            }
        }
    }

    /// Given a next module path and a path to the current module, decide on a file in the project
    /// to actually parse.
    fn find_next_path(
        &self,
        is_abs: bool,
        path_strs: &[String],
        mod_path: &[String],
        span: &Span,
    ) -> Option<(FoundPath, (PathBuf, Vec<String>))> {
        // Collect the module path as a vector of `String`s. Also, collect the next file path
        // starting from the project root path.
        let mut next_path = self.proj_root_path.clone();
        let mut next_mod_path = Vec::new();
        if !is_abs {
            for m in mod_path {
                next_path.push(m);
                next_mod_path.push(m.clone());
            }
        }
        for m in path_strs {
            next_path.push(m);
            next_mod_path.push(m.clone());
        }

        // An alternative next path includes a folder with the same name as the module.
        let alternative_next_path = next_path
            .join(next_mod_path.last().unwrap_or(&String::new()))
            .with_extension("pnt");

        next_path.set_extension("pnt");

        // Check if the paths actually exist.
        let next_path = next_path.exists().then_some(next_path);
        let alternative_next_path = alternative_next_path
            .exists()
            .then_some(alternative_next_path);

        // The path might refer to an external dependency.
        let dep_next_path = self.find_dep_path(path_strs);

        // Determine which of the file paths above to actually parse. If two files exist,
        // that's an error. If only 1 exists, return that one. If no paths exist, simply return
        // None.
        let next_path_opt = match (next_path, alternative_next_path, dep_next_path) {
            (None, None, None) => None,
            // One of the local paths exist.
            (Some(p), None, None) | (None, Some(p), None) => Some((FoundPath::Local, p)),
            // The dependency path exists.
            (None, None, Some(p)) => Some((FoundPath::Dep, p)),
            // Two or more potential paths exist, meaning we have an ambiguity.
            (Some(a), Some(b), _) | (Some(a), _, Some(b)) | (_, Some(a), Some(b)) => {
                self.handler.emit_err(Error::Compile {
                    error: CompileError::DualModulity {
                        mod_path: next_mod_path.join("::"),
                        file_path_a: a,
                        file_path_b: b,
                        span: span.clone(),
                    },
                });
                None
            }
        };

        next_path_opt.map(|(found, path)| (found, (path, next_mod_path)))
    }

    /// Attempt to find a path to a dependency module associated with the given `path_strs`.
    ///
    /// Returns `None` in the case there is no matching dependency, or the expected `Path`
    /// does not exist.
    fn find_dep_path(&self, path_strs: &[String]) -> Option<PathBuf> {
        let entry_point = path_strs
            .first()
            .and_then(|name| self.deps.get(name.as_str()))?;
        // By default, the path is the dep's entrypoint (e.g. `dep/src/lib.pnt`).
        let mut path = entry_point.to_path_buf();
        // If the path accesses some submodule of the dep, construct the
        // path via the dep's entry point parent directory.
        if path_strs.len() > 1 {
            assert!(path.pop(), "dep entrypoint has no parent dir");
            path.extend(&path_strs[1..]);
            path = path.with_extension("pnt");
        }
        path.exists().then_some(path)
    }
}

pub(crate) enum TestWrapper {
    Expr(ExprKey),
    Type(Type),
    Ident(Ident),
    #[allow(dead_code)]
    UseTree(UseTree),
}

impl TestWrapper {
    #[allow(dead_code)]
    fn gather_paths(&self) -> Vec<UsePath> {
        let TestWrapper::UseTree(u) = self else {
            panic!("Must call gather_paths() only for UseTree tests.");
        };
        u.gather_paths()
    }
}

impl crate::predicate::DisplayWithPred for TestWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter, pred: &Predicate) -> std::fmt::Result {
        match self {
            TestWrapper::Expr(e) => e.fmt(f, pred),
            TestWrapper::Type(t) => t.fmt(f, pred),
            TestWrapper::Ident(i) => i.fmt(f, pred),
            TestWrapper::UseTree(_) => panic!("DisplayWithPred not avilable for UseTree"),
        }
    }
}
