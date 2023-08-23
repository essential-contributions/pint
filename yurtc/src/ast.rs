use crate::{
    contract::{ContractDecl as CD, InterfaceDecl as ID},
    error::Span,
    expr::Expr as E,
    types::{EnumDecl, FnSig as F, Type as T},
};

pub(super) type Expr = E<Path, Block>;
pub(super) type Type = T<Path, Expr>;
pub(super) type FnSig = F<Type>;
pub(super) type InterfaceDecl = ID<Type>;
pub(super) type ContractDecl = CD<Path, Expr, Type>;

#[derive(Clone, Debug, PartialEq)]
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
        directive: SolveFunc,
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

#[derive(Clone, Debug, PartialEq)]
pub(super) enum UseTree {
    Glob,
    Name { name: Ident },
    Path { prefix: Ident, suffix: Box<UseTree> },
    Group { imports: Vec<UseTree> },
    Alias { name: Ident, alias: Ident },
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Block {
    pub(super) statements: Vec<Decl>,
    pub(super) final_expr: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Ident {
    pub(super) name: String,
    pub(super) span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Path {
    pub(super) path: Vec<Ident>,
    pub(super) is_absolute: bool,
    pub(super) span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum SolveFunc {
    Satisfy,
    Minimize(Path),
    Maximize(Path),
}
