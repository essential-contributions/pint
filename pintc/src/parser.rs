use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler, ParseError},
    expr::{Expr, Ident},
    intermediate::{CallKey, ExprKey, IntermediateIntent, Program},
    lexer,
    macros::{self, MacroCall, MacroDecl, MacroExpander},
    span::{self, empty_span, Span},
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
use slotmap::SlotMap;
pub(crate) use use_path::{UsePath, UseTree};

mod context;
pub(crate) use context::ParserContext;

#[cfg(test)]
mod tests;

pub fn parse_project(handler: &Handler, root_src_path: &Path) -> Result<Program, ErrorEmitted> {
    ProjectParser::new(handler, PathBuf::from(root_src_path))
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
    unique_idx: u64,
}

#[derive(Clone)]
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

impl<'a> ProjectParser<'a> {
    fn new(handler: &'a Handler, root_src_path: PathBuf) -> Self {
        let proj_root_path = root_src_path
            .parent()
            .map_or_else(|| PathBuf::from("/"), PathBuf::from);

        let mut project_parser = Self {
            program: Program::default(),
            macros: vec![],
            macro_calls: BTreeMap::from([(
                Program::ROOT_II_NAME.to_string(),
                slotmap::SecondaryMap::default(),
            )]),
            proj_root_path,
            root_src_path,
            visited_paths: vec![],
            handler,
            unique_idx: 0,
        };

        // Start with an empty II with an empty name. This is the "root intent".
        project_parser.program.iis.insert(
            Program::ROOT_II_NAME.to_string(),
            IntermediateIntent::default(),
        );

        project_parser
    }

    /// Parse the project starting with the `root_src_path` and return an intermediate intent. Upon
    /// failure, return a vector of all compile errors encountered.
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
            for current_ii in &keys {
                let macro_calls = self.macro_calls.get_mut(current_ii).unwrap();

                // Expand the next call. It may find new paths.
                if let Some(call_key) = macro_calls.keys().nth(0) {
                    let (call_expr_key, mut call) = macro_calls
                        .remove(call_key)
                        .expect("Call key must be valid.");

                    macros::splice_args(
                        self.handler,
                        self.program
                            .iis
                            .get(current_ii)
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
                            current_ii.clone(),
                        );

                        self.analyse_and_add_paths(&call.mod_path, &next_paths, &mut pending_paths);

                        call_replacements.push((
                            current_ii.clone(),
                            call_expr_key,
                            body_expr,
                            call.span.clone(),
                        ));
                    }
                }
            }

            for current_ii in keys {
                let macro_calls = self.macro_calls.get_mut(&current_ii).unwrap();
                if macro_calls.is_empty() {
                    self.macro_calls.remove(&current_ii);
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
                        span: span::empty_span(),
                    },
                });
                return self;
            }
        }

        // For each call we need to replace its user (there should be just one) with the macro body
        // expression.  Or, for macros which only had declarations and no body expression, just
        // delete the call.
        for (current_ii, call_expr_key, body_expr_key, span) in call_replacements {
            let ii = self.program.iis.get_mut(&current_ii).unwrap();

            if let Some(body_expr_key) = body_expr_key {
                ii.replace_exprs(call_expr_key, body_expr_key);
            } else {
                // Keep track of the removed macro for type-checking, in case the intent
                // erroneously expected the macro call to be an expression (and not just
                // declarations).
                ii.removed_macro_calls.insert(call_expr_key, span);
            }
            ii.exprs.remove(call_expr_key);
        }

        self
    }

    fn finalize(mut self) -> Result<Program, ErrorEmitted> {
        macro_rules! process_nested_expr {
            ($expr_key: expr, $error_msg: literal, $root_exprs: expr, $ii: expr, $handler: expr) => {{
                let nested_expr = $root_exprs.get(*$expr_key).ok_or_else(|| {
                    $handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: concat!("missing ", $error_msg, " expr key in exprs slotmap"),
                            span: empty_span(),
                        },
                    })
                })?;
                $ii.exprs.insert(nested_expr.clone());
                deep_copy_expr(nested_expr, $root_exprs, $ii, $handler)
            }};
        }

        fn deep_copy_expr(
            expr: &Expr,
            root_exprs: &SlotMap<ExprKey, Expr>,
            ii: &mut IntermediateIntent,
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
                Expr::UnaryOp { expr, .. } => {
                    process_nested_expr!(expr, "unary op", root_exprs, ii, handler)?;
                }
                Expr::BinaryOp { lhs, rhs, .. } => {
                    process_nested_expr!(lhs, "`lhs` of binary op", root_exprs, ii, handler)?;
                    process_nested_expr!(rhs, "`rhs` of binary op", root_exprs, ii, handler)?;
                }
                Expr::IntrinsicCall { args, .. } => {
                    for arg_expr_key in args {
                        process_nested_expr!(
                            arg_expr_key,
                            "intrinsic call `arg`",
                            root_exprs,
                            ii,
                            handler
                        )?;
                    }
                }
                Expr::If {
                    condition,
                    then_block,
                    else_block,
                    ..
                } => {
                    process_nested_expr!(condition, "if `condition`", root_exprs, ii, handler)?;
                    process_nested_expr!(then_block, "if `then block`", root_exprs, ii, handler)?;
                    process_nested_expr!(else_block, "if `else block`", root_exprs, ii, handler)?;
                }
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
                            ii,
                            handler
                        )?;
                    }
                    process_nested_expr!(range_expr, "array `range`", root_exprs, ii, handler)?;
                }
                Expr::Index { expr, index, .. } => {
                    process_nested_expr!(expr, "index `expr`", root_exprs, ii, handler)?;
                    process_nested_expr!(index, "index `index`", root_exprs, ii, handler)?;
                }
                Expr::Tuple { fields, .. } => {
                    for (_, field_expr_key) in fields {
                        process_nested_expr!(
                            field_expr_key,
                            "tuple `field`",
                            root_exprs,
                            ii,
                            handler
                        )?;
                    }
                }
                Expr::TupleFieldAccess { tuple, .. } => {
                    process_nested_expr!(tuple, "tuple field access", root_exprs, ii, handler)?;
                }
                Expr::Cast { value, ty, .. } => {
                    process_nested_expr!(value, "cast `value`", root_exprs, ii, handler)?;
                    deep_copy_type(ty, root_exprs, ii, handler)?;
                }
                Expr::In {
                    value, collection, ..
                } => {
                    process_nested_expr!(value, "in `value`", root_exprs, ii, handler)?;
                    process_nested_expr!(collection, "in `collection`", root_exprs, ii, handler)?;
                }
                Expr::Range { lb, ub, .. } => {
                    process_nested_expr!(lb, "range `lower bound`", root_exprs, ii, handler)?;
                    process_nested_expr!(ub, "range `upper bound`", root_exprs, ii, handler)?;
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
                            ii,
                            handler
                        )?;
                    }
                    for condition_expr_key in conditions {
                        process_nested_expr!(
                            condition_expr_key,
                            "generator `condition`",
                            root_exprs,
                            ii,
                            handler
                        )?;
                    }
                    process_nested_expr!(body, "generator `body`", root_exprs, ii, handler)?;
                }
            }
            Ok(())
        }

        fn deep_copy_type(
            new_type: &Type,
            root_exprs: &SlotMap<ExprKey, Expr>,
            ii: &mut IntermediateIntent,
            handler: &Handler,
        ) -> Result<Type, ErrorEmitted> {
            match &new_type {
                Type::Array {
                    ty,
                    range,
                    size,
                    span,
                } => {
                    let range_expr = root_exprs.get(*range).expect("exists");

                    deep_copy_expr(range_expr, root_exprs, ii, handler)?;
                    let new_expr_key = ii.exprs.insert(range_expr.clone());

                    Ok(Type::Array {
                        ty: Box::new(deep_copy_type(ty, root_exprs, ii, handler)?),
                        range: new_expr_key,
                        size: *size,
                        span: span.clone(),
                    })
                }
                Type::Tuple { fields, span } => {
                    let mut new_fields: Vec<(Option<Ident>, Type)> = vec![];
                    for field in fields {
                        let new_field = (
                            field.0.clone(),
                            deep_copy_type(&field.1, root_exprs, ii, handler)?,
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
                    ty: Box::new(deep_copy_type(ty, root_exprs, ii, handler)?),
                    span: span.clone(),
                }),
                Type::Map {
                    ty_from,
                    ty_to,
                    span,
                } => Ok(Type::Map {
                    ty_from: Box::new(deep_copy_type(ty_from, root_exprs, ii, handler)?),
                    ty_to: Box::new(deep_copy_type(ty_to, root_exprs, ii, handler)?),
                    span: span.clone(),
                }),
                Type::Error(_) | Type::Primitive { .. } | Type::Custom { .. } => {
                    Ok(new_type.clone())
                }
            }
        }

        fn deep_copy_new_types(
            root_new_types: &Vec<NewTypeDecl>,
            root_exprs: &SlotMap<ExprKey, Expr>,
            ii: &mut IntermediateIntent,
            handler: &Handler,
        ) -> Result<(), ErrorEmitted> {
            for new_type in root_new_types {
                let new_type_decl = deep_copy_type(&new_type.ty, root_exprs, ii, handler)?;
                ii.new_types.push(NewTypeDecl {
                    name: new_type.name.clone(),
                    ty: new_type_decl,
                    span: new_type.span.clone(),
                })
            }
            Ok(())
        }

        let enums = self.program.root_ii().enums.clone();
        let new_types = self.program.root_ii().new_types.clone();
        let root_symbols = self.program.root_ii().top_level_symbols.clone();
        let storage = self.program.root_ii().storage.clone();
        let externs = self.program.root_ii().externs.clone();
        let exprs = self.program.root_ii().exprs.clone();

        self.program
            .iis
            .iter_mut()
            .filter(|(name, _)| *name != &Program::ROOT_II_NAME.to_string())
            .for_each(|(_, ii)| {
                let _ = deep_copy_new_types(&new_types, &exprs, ii, self.handler);
                ii.enums.extend_from_slice(&enums);
                ii.storage = storage.clone();
                ii.externs = externs.clone();

                for (symbol, span) in &root_symbols {
                    ii.top_level_symbols
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
                            ii.top_level_symbols.insert(symbol.clone(), span.clone());
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
     $current_ii: expr,
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
        let mut current_ii = $current_ii.to_string();

        let mut next_paths = Vec::new();

        let mut context = ParserContext {
            $mod_path,
            mod_prefix: &mod_prefix,
            local_scope: $local_scope,
            program: &mut $self.program,
            current_ii: &mut current_ii,
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
                    span: span::empty_span(),
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
            Program::ROOT_II_NAME, // The II when we explore a new module is always the root II
            self.handler,
        )
    }

    fn parse_macro_body(
        &mut self,
        tokens: Vec<(usize, lexer::Token, usize)>,
        src_path: &Rc<Path>,
        mod_path: &[String],
        macro_call: &MacroCall,
        current_ii: String,
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
            current_ii,
            self.handler,
        )
    }

    fn analyse_and_add_paths(
        &mut self,
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

            if let Some(next_path) = self.find_next_path(*is_abs, mod_path_strs, mod_path, span) {
                pending_paths.push(next_path);
                continue;
            }

            if let Some(enum_path_strs) = enum_path_strs {
                if let Some(next_path) =
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
        &mut self,
        is_abs: bool,
        path_strs: &[String],
        mod_path: &[String],
        span: &Span,
    ) -> Option<(PathBuf, Vec<String>)> {
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

        // Determine which of the two file paths above to actually parse. If both files exist,
        // that's an error. If only 1 exists, return that one. If no paths exist, simply return
        // None.
        match (next_path.exists(), alternative_next_path.exists()) {
            (false, false) => None,
            (true, false) => Some((next_path, next_mod_path)),
            (false, true) => Some((alternative_next_path, next_mod_path)),
            (true, true) => {
                self.handler.emit_err(Error::Compile {
                    error: CompileError::DualModulity {
                        mod_path: next_mod_path.join("::"),
                        file_path_a: next_path,
                        file_path_b: alternative_next_path,
                        span: span.clone(),
                    },
                });
                None
            }
        }
    }
}
