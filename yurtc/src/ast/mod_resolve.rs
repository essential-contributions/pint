use super::{
    use_path::UsePath, Ast, Block, ContractDecl, Decl, Expr, FnSig, Ident, InterfaceDecl, Path,
    SolveFunc, Type,
};
use crate::{
    error::{CompileError, Error},
    expr::{CondBranch, TupleAccess},
    parser::parse_str_to_ast,
    span,
    types::EnumDecl,
};

use std::{
    cell::RefCell,
    collections::{
        btree_map::{self, Entry},
        BTreeMap,
    },
    fs, path,
    rc::Rc,
};

/// Parse a path to the _root_ module into ASTs.  Each root AST is recursively
/// inspected and any referenced sub-modules are also parsed _and merged_ into the project AST, and
/// then returned.
pub fn parse_project(source_path: &std::path::Path) -> Result<super::Ast, Vec<Error>> {
    let mut resolver = ModuleResolver::default();
    resolver.load_source(SourcePath::FileName(source_path.into()), Vec::new())?;
    resolver.finish()
}

/// Used to resolve all modules in a project and to create a single AST containing all the modules
/// with absolute symbols.
///
/// Root sources for a project (taken e.g. from the CLI) can be added using `load_source()` and
/// then calling `finish()` will inspect those sources, recursively load referred modules and
/// canonicalise all relative symbols into absolute paths from the root of the project.
///
/// Once all modules are loaded and canonical they're merged into a single project AST.

#[derive(Debug, Default)]
struct ModuleResolver {
    sources: BTreeMap<Rc<path::Path>, SourceKey>,
    next_key: SourceKey,

    next_anon_idx: RefCell<u64>,

    asts: BTreeMap<SourceKey, Ast>,
    parent_paths: BTreeMap<SourceKey, Option<path::PathBuf>>,
    mod_paths: BTreeMap<SourceKey, Vec<Ident>>,
    top_level_decls: BTreeMap<SourceKey, Vec<Ident>>,

    uses: BTreeMap<SourceKey, Vec<UsePath>>,

    final_decls: BTreeMap<SourceKey, Vec<Decl>>,
}

/// ModuleResolver is a poor-man's content management system, where we have a bunch of data all
/// using the same key.  This macro will fetch a value for when we assume the item exists.
macro_rules! resolver_get {
    ($map: expr, $key: expr, $descr: literal) => {
        $map.get($key).ok_or_else(|| Error::Compile {
            error: CompileError::Internal {
                msg: concat!($descr, " must already exist for key in module resolver."),
                span: span::empty_span(),
            },
        })
    };
}
macro_rules! resolver_getv {
    ($map: expr, $key: expr, $descr: literal) => {
        resolver_get!($map, $key, $descr).map_err(|e| vec![e])
    };
}

/// Each module gets a unique key used to reference its components within the resolver.
type SourceKey = usize;

/// SourcePath::FileName specifies a Yurt file to load.  SourcePath::Module specifies a module
/// which could be either a directory 'a' which must contain 'a/a.yrt' or just a file 'a' which
/// refers to 'a.yrt'.
enum SourcePath {
    FileName(path::PathBuf),
    Module(path::PathBuf),
}

impl ModuleResolver {
    /// Return whether module was actually found (and loaded).
    fn load_source(
        &mut self,
        relative_path: SourcePath,
        module_path: Vec<Ident>,
    ) -> Result<bool, Vec<Error>> {
        let wrap_io_error = |io_err, file| {
            vec![Error::Compile {
                error: CompileError::FileIO {
                    error: io_err,
                    file,
                    span: span::empty_span(),
                },
            }]
        };

        let src_path: Rc<path::Path> = match relative_path {
            SourcePath::FileName(p) => {
                Rc::from(fs::canonicalize(p.clone()).map_err(|e| wrap_io_error(e, Some(p)))?)
            }
            SourcePath::Module(p) => {
                // `p` must be to a module 'a/b/c' where we expect either 'a/b/c.yrt' to exist, or
                // 'a/b/c/c.yrt'.

                let mut file_mod_path = p.clone();
                file_mod_path.set_extension("yrt");
                let file_mod_path_exists = file_mod_path.exists();

                let dir_mod_path = p.file_name().map(|fname| {
                    let mut dmp = p.clone();
                    dmp.push(fname);
                    dmp.set_extension("yrt");
                    dmp
                });
                let dir_mod_path_exists = dir_mod_path
                    .as_ref()
                    .map(|fname| fname.exists())
                    .unwrap_or(false);

                Rc::from(match (file_mod_path_exists, dir_mod_path_exists) {
                    (true, true) => Err(vec![Error::Compile {
                        error: CompileError::DualModulity {
                            path: p,
                            path_a: file_mod_path,
                            path_b: dir_mod_path.unwrap(),
                            span: span::empty_span(),
                        },
                    }]),

                    (false, false) => return Ok(false),

                    (true, false) => fs::canonicalize(file_mod_path.clone())
                        .map_err(|e| wrap_io_error(e, Some(file_mod_path))),

                    (false, true) => fs::canonicalize(dir_mod_path.as_ref().unwrap().clone())
                        .map_err(|e| wrap_io_error(e, dir_mod_path)),
                }?)
            }
        };

        if let Entry::Vacant(sources) = self.sources.entry(src_path.clone()) {
            let key = self.next_key;
            self.next_key += 1;

            let src = fs::read_to_string(&src_path)
                .map_err(|e| wrap_io_error(e, Some(src_path.to_path_buf())))?;
            let file_path = src_path.clone();
            let ast = parse_str_to_ast(&src, file_path)?;

            self.asts.insert(key, ast);
            self.parent_paths
                .insert(key, src_path.parent().map(|p| p.to_path_buf()));
            self.mod_paths.insert(key, module_path);
            sources.insert(key);
        }

        Ok(true)
    }

    fn finish(mut self) -> Result<super::Ast, Vec<Error>> {
        loop {
            // Find an AST which hasn't been canonicalised yet.
            let key = self
                .asts
                .keys()
                .find(|&p| !self.final_decls.contains_key(p));
            if key.is_none() {
                break;
            }
            let key = key.copied().unwrap();

            // Get its uses list.
            self.load_uses(key)?;

            // Also gather its top-level declaration names.
            self.gather_top_level_decl_syms(key)?;

            // Rewrite the AST with absolute paths.
            let mod_path = resolver_getv!(self.mod_paths, &key, "Module path")?;
            let absolute_decls = resolver_getv!(self.asts, &key, "AST")?
                .iter()
                .filter_map(|decl| {
                    self.to_absolute(decl, &key, mod_path)
                        .map_err(|e| vec![e])
                        .transpose()
                })
                .collect::<Result<Vec<Decl>, _>>()?;

            // Gather all the new absolute paths and find new modules which need loading.
            let parent_path = resolver_getv!(self.parent_paths, &key, "Parent path")?
                .clone()
                .unwrap_or("".into());
            for (src_file_path, src_mod_path) in Paths::from_ast(&absolute_decls).iter(mod_path) {
                // `load_source()` will ignore already loaded source files.  If the module can't be
                // found (it may actually be the path to an enum variant) then just ignore it.
                // Further semantic analysis will fail with an unresolved symbol error if it is
                // supposed to be in a missing module.
                let full_src_file_path = path::PathBuf::from_iter([&parent_path, &src_file_path]);
                self.load_source(SourcePath::Module(full_src_file_path), src_mod_path)?;
            }

            // Save this canonical AST.
            self.final_decls.insert(key, absolute_decls);
        }

        Ok(Ast(self.final_decls.into_values().flatten().collect()))
    }

    fn load_uses(&mut self, src_key: SourceKey) -> Result<(), Vec<Error>> {
        if self.uses.contains_key(&src_key) {
            return Ok(());
        }

        let ast = resolver_getv!(self.asts, &src_key, "AST")?;
        let mod_path = resolver_getv!(self.mod_paths, &src_key, "module path")?;

        let uses = ast
            .iter()
            .filter_map(|decl| match decl {
                Decl::Use {
                    use_tree,
                    is_absolute,
                    ..
                } => Some(
                    use_tree
                        .gather_paths()
                        .into_iter()
                        .map(move |mut use_path| {
                            if !is_absolute {
                                use_path.add_prefix(mod_path.clone());
                            }
                            use_path
                        }),
                ),
                _ => None,
            })
            .flatten()
            .collect();

        self.uses.insert(src_key, uses);

        // If the top-levels are already loaded we should check for a name clash with these new
        // uses.
        self.check_for_use_name_clash(src_key)
    }

    fn gather_top_level_decl_syms(&mut self, src_key: SourceKey) -> Result<(), Vec<Error>> {
        if self.top_level_decls.contains_key(&src_key) {
            return Ok(());
        }

        let ast = resolver_getv!(self.asts, &src_key, "AST")?;

        let mut top_levels = Vec::new();
        for decl in ast {
            match decl {
                Decl::Let { name, .. }
                | Decl::State { name, .. }
                | Decl::Fn {
                    fn_sig: FnSig { name, .. },
                    ..
                }
                | Decl::Enum(super::EnumDecl { name, .. })
                | Decl::NewType { name, .. }
                | Decl::Interface(super::InterfaceDecl { name, .. })
                | Decl::Contract(super::ContractDecl { name, .. }) => top_levels.push(name.clone()),

                Decl::Extern { functions, .. } => {
                    top_levels.extend(functions.iter().map(|FnSig { name, .. }| name.clone()));
                }

                Decl::Use { .. } | Decl::Constraint { .. } | Decl::Solve { .. } => (),
            }
        }

        self.top_level_decls.insert(src_key, top_levels);

        // If the uses are already loaded we should check for a name clash with these new
        // top-levels.
        self.check_for_use_name_clash(src_key)
    }

    fn check_for_use_name_clash(&self, src_key: SourceKey) -> Result<(), Vec<Error>> {
        if !(self.top_level_decls.contains_key(&src_key) && self.uses.contains_key(&src_key)) {
            // We need both to already be loaded.
            return Ok(());
        }

        let top_level_decls = resolver_getv!(self.top_level_decls, &src_key, "Top level decls")?;
        let uses = resolver_getv!(self.uses, &src_key, "Uses")?;

        // Do an N^2 search for clashes between top-levels and `use` imports.
        let errors = top_level_decls
            .iter()
            .flat_map(|top_level_id| {
                uses.iter().filter_map(move |use_path| {
                    use_path.path.last().and_then(|last_element| {
                        (last_element == top_level_id).then(|| Error::Compile {
                            error: CompileError::NameClash {
                                sym: last_element.to_string(),
                                span: top_level_id.span.clone(),
                                prev_span: use_path.span.clone(),
                            },
                        })
                    })
                })
            })
            .collect::<Vec<_>>();

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(())
        }
    }

    // A lot of cloning here.  Ideally the idents, which are the only thing we're rewriting, would
    // be externalised into a symbol table already.  But the AST and parser need to support that.
    fn to_absolute(
        &self,
        decl: &Decl,
        key: &SourceKey,
        mod_path: &[Ident],
    ) -> Result<Option<Decl>, Error> {
        let convert_fn_sig = |fn_sig: &FnSig, local_name: Option<&Ident>| {
            self.convert_ident(mod_path, &fn_sig.name, local_name)
                .map(|name| FnSig {
                    name,
                    ..fn_sig.clone()
                })
        };

        Ok(match decl {
            Decl::Use { .. } => None,

            Decl::Let {
                name,
                ty,
                init,
                span,
            } => Some(Decl::Let {
                name: self.convert_ident(mod_path, name, None)?,
                ty: ty.clone(),
                init: init
                    .as_ref()
                    .map(|expr| self.convert_expr(key, mod_path, expr))
                    .transpose()?,
                span: span.clone(),
            }),

            Decl::State {
                name,
                ty,
                init,
                span,
            } => Some(Decl::State {
                name: self.convert_ident(mod_path, name, None)?,
                ty: ty.clone(),
                init: self.convert_expr(key, mod_path, init)?,
                span: span.clone(),
            }),

            Decl::Constraint { expr, span } => Some(Decl::Constraint {
                expr: self.convert_expr(key, mod_path, expr)?,
                span: span.clone(),
            }),

            Decl::Fn { fn_sig, body, span } => Some(Decl::Fn {
                fn_sig: convert_fn_sig(fn_sig, None)?,
                body: self.convert_block(key, mod_path, body, Some(&fn_sig.name))?,
                span: span.clone(),
            }),

            Decl::Solve { directive, span } => Some(Decl::Solve {
                directive: match directive {
                    SolveFunc::Satisfy => directive.clone(),
                    SolveFunc::Minimize(expr) => {
                        SolveFunc::Minimize(self.convert_expr(key, mod_path, expr)?)
                    }
                    SolveFunc::Maximize(expr) => {
                        SolveFunc::Maximize(self.convert_expr(key, mod_path, expr)?)
                    }
                },
                span: span.clone(),
            }),

            Decl::Enum(enum_decl) => Some(Decl::Enum(EnumDecl {
                name: self.convert_ident(mod_path, &enum_decl.name, None)?,
                ..enum_decl.clone()
            })),

            Decl::NewType { name, ty, span } => Some(Decl::NewType {
                name: self.convert_ident(mod_path, name, None)?,
                ty: ty.clone(),
                span: span.clone(),
            }),

            Decl::Interface(InterfaceDecl {
                name,
                functions,
                span,
            }) => Some(Decl::Interface(InterfaceDecl {
                name: self.convert_ident(mod_path, name, None)?,
                functions: functions
                    .iter()
                    .map(|fn_sig| convert_fn_sig(fn_sig, Some(name)))
                    .collect::<Result<_, _>>()?,
                span: span.clone(),
            })),

            Decl::Contract(ContractDecl {
                name,
                id,
                interfaces,
                functions,
                span,
            }) => Some(Decl::Contract(ContractDecl {
                name: self.convert_ident(mod_path, name, None)?,
                id: self.convert_expr(key, mod_path, id)?,
                interfaces: interfaces
                    .iter()
                    .map(|iface| self.convert_path(key, mod_path, iface))
                    .collect::<Result<_, _>>()?,
                functions: functions
                    .iter()
                    .map(|fn_sig| convert_fn_sig(fn_sig, Some(name)))
                    .collect::<Result<_, _>>()?,
                span: span.clone(),
            })),

            Decl::Extern { functions, span } => Some(Decl::Extern {
                functions: functions
                    .iter()
                    .map(|fn_sig| convert_fn_sig(fn_sig, None))
                    .collect::<Result<_, _>>()?,
                span: span.clone(),
            }),
        })
    }

    fn convert_expr(
        &self,
        key: &SourceKey,
        mod_path: &[Ident],
        expr: &Expr,
    ) -> Result<Expr, Error> {
        Ok(match expr {
            Expr::Immediate { .. } => expr.clone(),

            Expr::Path(p) => Expr::Path(self.convert_path(key, mod_path, p)?),

            Expr::UnaryOp { op, expr, span } => Expr::UnaryOp {
                op: *op,
                expr: Box::new(self.convert_expr(key, mod_path, expr)?),
                span: span.clone(),
            },

            Expr::BinaryOp { op, lhs, rhs, span } => Expr::BinaryOp {
                op: *op,
                lhs: Box::new(self.convert_expr(key, mod_path, lhs)?),
                rhs: Box::new(self.convert_expr(key, mod_path, rhs)?),
                span: span.clone(),
            },

            Expr::Call { name, args, span } => Expr::Call {
                name: self.convert_path(key, mod_path, name)?,
                args: args
                    .iter()
                    .map(|arg| self.convert_expr(key, mod_path, arg))
                    .collect::<Result<_, _>>()?,
                span: span.clone(),
            },

            Expr::Block(block) => {
                let anon_id = self.get_anon_ident(block.span.clone());
                Expr::Block(self.convert_block(key, mod_path, block, Some(&anon_id))?)
            }

            Expr::If {
                condition,
                then_block,
                else_block,
                span,
            } => Expr::If {
                condition: Box::new(self.convert_expr(key, mod_path, condition)?),
                then_block: self.convert_block(key, mod_path, then_block, None)?,
                else_block: self.convert_block(key, mod_path, else_block, None)?,
                span: span.clone(),
            },

            Expr::Cond {
                branches,
                else_result,
                span,
            } => Expr::Cond {
                branches: branches
                    .iter()
                    .map(|cond| {
                        self.convert_expr(key, mod_path, &cond.condition)
                            .and_then(|c| {
                                self.convert_expr(key, mod_path, &cond.result)
                                    .map(|r| CondBranch {
                                        condition: Box::new(c),
                                        result: Box::new(r),
                                        span: cond.span.clone(),
                                    })
                            })
                    })
                    .collect::<Result<_, _>>()?,
                else_result: Box::new(self.convert_expr(key, mod_path, else_result)?),
                span: span.clone(),
            },

            Expr::Array { elements, span } => Expr::Array {
                elements: elements
                    .iter()
                    .map(|el| self.convert_expr(key, mod_path, el))
                    .collect::<Result<_, _>>()?,
                span: span.clone(),
            },

            Expr::ArrayElementAccess { array, index, span } => Expr::ArrayElementAccess {
                array: Box::new(self.convert_expr(key, mod_path, array)?),
                index: Box::new(self.convert_expr(key, mod_path, index)?),
                span: span.clone(),
            },

            Expr::Tuple { fields, span } => Expr::Tuple {
                fields: fields
                    .iter()
                    .map(|(opt_name, val)| {
                        opt_name
                            .as_ref()
                            .map(|name| self.convert_ident(mod_path, name, None))
                            .transpose()
                            .and_then(|n| self.convert_expr(key, mod_path, val).map(|v| (n, v)))
                    })
                    .collect::<Result<_, _>>()?,
                span: span.clone(),
            },

            Expr::TupleFieldAccess { tuple, field, span } => Expr::TupleFieldAccess {
                tuple: Box::new(self.convert_expr(key, mod_path, tuple)?),
                field: match field {
                    TupleAccess::Index(_) => field.clone(),
                    TupleAccess::Name(id) => {
                        TupleAccess::Name(self.convert_ident(mod_path, id, None)?)
                    }
                },
                span: span.clone(),
            },

            Expr::Cast { value, ty, span } => Expr::Cast {
                value: Box::new(self.convert_expr(key, mod_path, value)?),
                ty: Box::new(self.convert_type(key, mod_path, ty)?),
                span: span.clone(),
            },

            Expr::In {
                value,
                collection,
                span,
            } => Expr::In {
                value: Box::new(self.convert_expr(key, mod_path, value)?),
                collection: Box::new(self.convert_expr(key, mod_path, collection)?),
                span: span.clone(),
            },

            Expr::Range { lb, ub, span } => Expr::Range {
                lb: Box::new(self.convert_expr(key, mod_path, lb)?),
                ub: Box::new(self.convert_expr(key, mod_path, ub)?),
                span: span.clone(),
            },
        })
    }

    fn convert_block(
        &self,
        key: &SourceKey,
        mod_path: &[Ident],
        block: &Block,
        local_name: Option<&Ident>,
    ) -> Result<Block, Error> {
        let mut local_mod_path = mod_path.to_vec();
        if let Some(local_name) = local_name {
            local_mod_path.push(local_name.clone());
        }

        let statements = block
            .statements
            .iter()
            .filter_map(|stmt| self.to_absolute(stmt, key, &local_mod_path).transpose())
            .collect::<Result<_, _>>()?;

        Ok(Block {
            statements,
            final_expr: Box::new(self.convert_expr(key, mod_path, &block.final_expr)?),
            span: block.span.clone(),
        })
    }

    fn convert_ident(
        &self,
        mod_path: &[Ident],
        id: &Ident,
        local_name: Option<&Ident>,
    ) -> Result<Ident, Error> {
        let mod_path_prefix = Self::path_to_string(mod_path);

        let local_name_str = if let Some(local_name) = local_name {
            format!("::{local_name}")
        } else {
            String::new()
        };
        Ok(Ident {
            name: format!("{mod_path_prefix}{local_name_str}::{id}",),
            span: id.span.clone(),
        })
    }

    fn convert_path(
        &self,
        key: &SourceKey,
        mod_path: &[Ident],
        path: &Path,
    ) -> Result<Path, Error> {
        if path.is_absolute {
            return Ok(path.clone());
        }

        // See if the path is simply to a local decl.
        let top_levels = resolver_get!(self.top_level_decls, key, "Top level symbols")?;
        let is_local = path.path.len() == 1 && top_levels.iter().any(|sym| sym == &path.path[0]);

        // If not local, find a matching use path, by alias or by matching the last ident
        // in the use path with the first in the passed path.  (Does `use a::b` match
        // `b::c`?  Yes.)
        let uses = resolver_get!(self.uses, key, "Uses")?;
        let use_path: Option<&UsePath> = (!is_local)
            .then(|| {
                uses.iter().find(|use_path| {
                    use_path.alias.as_ref().map(|alias| alias == &path.path[0]) == Some(true)
                        || use_path.path.last() == Some(&path.path[0])
                })
            })
            .flatten();

        let mut abs_path = mod_path.to_vec();
        if let Some(use_path) = use_path {
            // Module path + use path + rest of passed path.
            // E.g., a.yrt + 'use b::c' + 'c::d' -> [a, b, c, d] (or ::a::b::c::d).
            abs_path.extend((use_path.path.iter().chain(path.path.iter().skip(1))).cloned());
        } else {
            // Either local or we couldn't find a matching use path. Module path + passed
            // path.
            // E.g., a.yrt + 'c::d' -> [a, c, d] (or ::a::c::d).
            abs_path.extend(path.path.iter().cloned());
        };

        Ok(Path {
            path: abs_path,
            is_absolute: true,
            span: path.span.clone(),
        })
    }

    fn convert_type(&self, key: &SourceKey, mod_path: &[Ident], ty: &Type) -> Result<Type, Error> {
        Ok(match ty {
            Type::Array { ty, range, span } => Type::Array {
                ty: ty.clone(),
                range: self.convert_expr(key, mod_path, range)?,
                span: span.clone(),
            },

            Type::CustomType { path, span } => Type::CustomType {
                path: self.convert_path(key, mod_path, path)?,
                span: span.clone(),
            },

            Type::Primitive { .. } | Type::Tuple { .. } => ty.clone(),
        })
    }

    fn get_anon_ident(&self, span: span::Span) -> Ident {
        let idx = self.next_anon_idx.replace_with(|&mut idx| idx + 1);
        let name = format!("@@{idx}");

        Ident { name, span }
    }

    fn path_to_string(path: &[Ident]) -> String {
        path.iter()
            .map(|el| format!("::{el}"))
            .collect::<Vec<_>>()
            .concat()
    }
}

/// A tree of Idents constructed from a collection of paths.  There's probably a crate for this,
/// but why depend on a 3rd party for something you can write in an hour?
#[derive(Clone, Debug, Default)]
struct Paths(BTreeMap<path::PathBuf, Vec<Ident>>);

impl Paths {
    fn from_ast(ast: &[Decl]) -> Self {
        fn insert_path(tree: &mut Paths, path: &[Ident]) {
            if path.len() > 1 {
                if let Some((_, path)) = path.split_last() {
                    let path_buf = path::PathBuf::from_iter(path.iter().map(|i| &i.name));
                    tree.0.insert(path_buf, path.to_vec());
                }
            }
        }

        fn from_expr(tree: &mut Paths, expr: &Expr) {
            match expr {
                Expr::Immediate { .. } => {}
                Expr::Path(path) => insert_path(tree, &path.path),
                Expr::UnaryOp { expr, .. } => from_expr(tree, expr),
                Expr::BinaryOp { lhs, rhs, .. } => {
                    from_expr(tree, lhs);
                    from_expr(tree, rhs);
                }
                Expr::Call { name, args, .. } => {
                    insert_path(tree, &name.path);
                    for arg in args {
                        from_expr(tree, arg);
                    }
                }
                Expr::Block(block) => from_block(tree, block),
                Expr::If {
                    condition,
                    then_block,
                    else_block,
                    ..
                } => {
                    from_expr(tree, condition);
                    from_block(tree, then_block);
                    from_block(tree, else_block);
                }
                Expr::Cond {
                    branches,
                    else_result,
                    ..
                } => {
                    for CondBranch {
                        condition, result, ..
                    } in branches
                    {
                        from_expr(tree, condition);
                        from_expr(tree, result);
                    }
                    from_expr(tree, else_result);
                }
                Expr::Array { elements, .. } => {
                    for element in elements {
                        from_expr(tree, element);
                    }
                }
                Expr::ArrayElementAccess { array, index, .. } => {
                    from_expr(tree, array);
                    from_expr(tree, index);
                }
                Expr::Tuple { .. } => {}
                Expr::TupleFieldAccess { tuple, .. } => from_expr(tree, tuple),
                Expr::Cast { value, ty, .. } => {
                    from_expr(tree, value);
                    from_type(tree, ty);
                }
                Expr::In {
                    value, collection, ..
                } => {
                    from_expr(tree, value);
                    from_expr(tree, collection);
                }
                Expr::Range { lb, ub, .. } => {
                    from_expr(tree, lb);
                    from_expr(tree, ub);
                }
            }
        }

        fn from_block(tree: &mut Paths, block: &Block) {
            for decl in &block.statements {
                from_decl(tree, decl);
            }
            from_expr(tree, &block.final_expr);
        }

        fn from_type(tree: &mut Paths, ty: &Type) {
            match ty {
                Type::Array { ty, range, .. } => {
                    from_type(tree, ty);
                    from_expr(tree, range);
                }

                Type::CustomType { path, .. } => {
                    insert_path(tree, &path.path);
                }

                Type::Tuple { .. } | Type::Primitive { .. } => {}
            }
        }

        fn from_fn_sig(tree: &mut Paths, fn_sig: &FnSig) {
            for (_, ty) in &fn_sig.params {
                from_type(tree, ty);
            }
            from_type(tree, &fn_sig.return_type);
        }

        fn from_decl(tree: &mut Paths, decl: &Decl) {
            match decl {
                Decl::Use { .. } | Decl::Enum(_) => {}

                Decl::Let { init, ty, .. } => {
                    if let Some(init) = init {
                        from_expr(tree, init);
                    }
                    if let Some(ty) = ty {
                        from_type(tree, ty);
                    }
                }

                Decl::State { ty, init, .. } => {
                    from_expr(tree, init);
                    if let Some(ty) = ty {
                        from_type(tree, ty);
                    }
                }

                Decl::Constraint { expr, .. } => from_expr(tree, expr),

                Decl::Fn { fn_sig, body, .. } => {
                    from_fn_sig(tree, fn_sig);
                    from_block(tree, body);
                }

                Decl::Solve { directive, .. } => match directive {
                    SolveFunc::Satisfy => {}
                    SolveFunc::Minimize(expr) | SolveFunc::Maximize(expr) => from_expr(tree, expr),
                },

                Decl::Interface(InterfaceDecl { functions, .. }) => {
                    for fn_sig in functions {
                        from_fn_sig(tree, fn_sig);
                    }
                }

                Decl::Contract(ContractDecl {
                    id,
                    interfaces,
                    functions,
                    ..
                }) => {
                    from_expr(tree, id);
                    for path in interfaces {
                        insert_path(tree, &path.path);
                    }
                    for fn_sig in functions {
                        from_fn_sig(tree, fn_sig);
                    }
                }

                Decl::NewType { ty, .. } => from_type(tree, ty),

                Decl::Extern { functions, .. } => {
                    for fn_sig in functions {
                        from_fn_sig(tree, fn_sig);
                    }
                }
            }
        }

        let mut tree = Paths::default();
        for decl in ast {
            from_decl(&mut tree, decl);
        }
        tree
    }

    fn iter(self, mod_path: &[Ident]) -> PathsIter {
        PathsIter {
            path_iter: self.0.into_iter(),
            base: path::PathBuf::from_iter(mod_path.iter().map(|i| &i.name)),
        }
    }
}

struct PathsIter {
    path_iter: btree_map::IntoIter<path::PathBuf, Vec<Ident>>,
    base: path::PathBuf,
}

impl Iterator for PathsIter {
    type Item = (path::PathBuf, Vec<Ident>);

    fn next(&mut self) -> Option<Self::Item> {
        self.path_iter.find(|(p, _)| p.starts_with(&self.base))
    }
}
