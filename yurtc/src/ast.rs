use crate::{error::Span, expr::Expr as E, types::Type as T};

pub(super) type Expr = E<Ident, Block>;
pub(super) type Type = T<Ident, Expr>;

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Decl {
    Use {
        is_absolute: bool,
        use_tree: UseTree,
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
    Enum {
        name: String,
        variants: Vec<String>,
        name_span: Span,
    },
    Interface {
        name: String,
        functions: Vec<FnSig>,
        name_span: Span,
    },
    Contract {
        name: String,
        id: Expr,
        interfaces: Vec<Ident>,
        functions: Vec<FnSig>,
        name_span: Span,
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
pub(super) struct FnSig {
    pub(super) name: String,
    pub(super) params: Vec<(String, Type)>,
    pub(super) return_type: Type,
    pub(super) span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Ident {
    pub(super) path: Vec<String>,
    pub(super) is_absolute: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum SolveFunc {
    Satisfy,
    Minimize(Ident),
    Maximize(Ident),
}
