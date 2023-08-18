use crate::{
    contract::{ContractDecl as CD, InterfaceDecl as ID},
    error::Span,
    expr::Expr as E,
    types::{EnumDecl, FnSig as F, Type as T},
};

pub(super) type Expr = E<Ident, Block>;
pub(super) type Type = T<Ident, Expr>;
pub(super) type FnSig = F<Type>;
pub(super) type InterfaceDecl = ID<Type>;
pub(super) type ContractDecl = CD<Ident, Expr, Type>;

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Decl {
    Use {
        is_absolute: bool,
        use_tree: UseTree,
        span: Span,
    },
    Let {
        name: String,
        ty: Option<Type>,
        init: Option<Expr>,
        span: Span,
    },
    State {
        name: String,
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
    },
    Solve {
        directive: SolveFunc,
        span: Span,
    },
    Enum(EnumDecl),
    Interface(InterfaceDecl),
    Contract(ContractDecl),
    Extern {
        functions: Vec<FnSig>,
        extern_keyword_span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum UseTree {
    Glob,
    Name {
        name: String,
    },
    Path {
        prefix: String,
        suffix: Box<UseTree>,
    },
    Group {
        imports: Vec<UseTree>,
    },
    Alias {
        name: String,
        alias: String,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Block {
    pub(super) statements: Vec<Decl>,
    pub(super) final_expr: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Ident {
    pub(super) path: Vec<String>,
    pub(super) is_absolute: bool,
    pub(super) span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum SolveFunc {
    Satisfy,
    Minimize(Ident),
    Maximize(Ident),
}
