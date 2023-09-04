use crate::{
    contract::{ContractDecl as CD, InterfaceDecl as ID},
    expr::Expr as E,
    span::{Span, Spanned},
    types::{EnumDecl, FnSig as FS, Type as T},
};

pub(super) type Expr = E<Path, Block>;
pub(super) type Type = T<Path, Expr>;
pub(super) type FnSig = FS<Type>;
pub(super) type InterfaceDecl = ID<Type>;
pub(super) type ContractDecl = CD<Path, Expr, Type>;

mod display;
mod mod_resolve;
mod use_path;

pub(super) use mod_resolve::parse_project;

#[derive(Clone, Debug)]
pub(crate) struct Ast(pub(crate) Vec<Decl>);

impl Ast {
    fn iter(&self) -> impl Iterator<Item = &Decl> {
        self.0.iter()
    }
}

impl<'a> IntoIterator for &'a Ast {
    type Item = &'a Decl;
    type IntoIter = std::slice::Iter<'a, Decl>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl IntoIterator for Ast {
    type Item = Decl;
    type IntoIter = <Vec<Decl> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Clone, Debug)]
pub(super) enum Decl {
    Use {
        is_absolute: bool,
        use_tree: UseTree,
        span: Span,
    },
    Let {
        name: Ident,
        ty: Option<Type>,
        init: Option<Expr>,
        span: Span,
    },
    State {
        name: Ident,
        ty: Option<Type>,
        init: Expr,
        span: Span,
    },
    Constraint {
        expr: Expr,
        span: Span,
    },
    Fn {
        fn_sig: FnSig,
        body: Block,
        span: Span,
    },
    Solve {
        directive: SolveFunc<Expr>,
        span: Span,
    },
    Enum(EnumDecl),
    Interface(InterfaceDecl),
    Contract(ContractDecl),
    NewType {
        name: Ident,
        ty: Type,
        span: Span,
    },
    Extern {
        functions: Vec<FnSig>,
        span: Span,
    },
}

impl Spanned for Decl {
    fn span(&self) -> &Span {
        use Decl::*;
        match &self {
            Use { span, .. }
            | Let { span, .. }
            | State { span, .. }
            | Constraint { span, .. }
            | Fn { span, .. }
            | Solve { span, .. }
            | Extern { span, .. } => span,
            Enum(enum_decl) => enum_decl.span(),
            Interface(interface_decl) => interface_decl.span(),
            Contract(contract_decl) => contract_decl.span(),
            NewType { span, .. } => span,
        }
    }
}

#[derive(Clone, Debug)]
pub(super) enum UseTree {
    Glob(Span),
    Name {
        name: Ident,
        span: Span,
    },
    Path {
        prefix: Ident,
        suffix: Box<UseTree>,
        span: Span,
    },
    Group {
        imports: Vec<UseTree>,
        span: Span,
    },
    Alias {
        name: Ident,
        alias: Ident,
        span: Span,
    },
}

impl Spanned for UseTree {
    fn span(&self) -> &Span {
        use UseTree::*;
        match &self {
            Glob(span) => span,
            Name { span, .. } | Path { span, .. } | Group { span, .. } | Alias { span, .. } => span,
        }
    }
}

#[derive(Clone, Debug)]
pub(super) struct Block {
    pub(super) statements: Vec<Decl>,
    pub(super) final_expr: Box<Expr>,
    pub(super) span: Span,
}

impl Spanned for Block {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug)]
pub(super) struct Ident {
    pub(super) name: String,
    pub(super) span: Span,
}

/// We want to compare identifiers fairly regularly, and we should not include the span.
impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Spanned for Ident {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug)]
pub(super) struct Path {
    pub(super) path: Vec<Ident>,
    pub(super) is_absolute: bool,
    pub(super) span: Span,
}

impl Spanned for Path {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug)]
pub(super) enum SolveFunc<Expr> {
    Satisfy,
    Minimize(Expr),
    Maximize(Expr),
}
