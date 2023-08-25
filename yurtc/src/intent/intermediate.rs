use crate::{
    ast,
    contract::{ContractDecl as CD, InterfaceDecl as ID},
    error::CompileError,
    expr::Expr as E,
    intent::{Intent, Path, Solve},
    span::Span,
    types::{EnumDecl, FnSig as F, Type as T},
};

mod compile;
mod from_ast;

type Result<T> = std::result::Result<T, CompileError>;

struct Block(Box<Expr>);
type Expr = E<Path, Block>;
type Type = T<Path, Expr>;
type FnSig = F<Type>;
type InterfaceDecl = ID<Type>;
type ContractDecl = CD<Path, Expr, Type>;

/// An in-progress intent, possibly malformed or containing redundant information.  Designed to be
/// iterated upon and to be reduced to an [Intent].
pub(super) struct IntermediateIntent {
    states: Vec<(State, Span)>,
    vars: Vec<(Var, Span)>,
    constraints: Vec<(Expr, Span)>,
    directives: Vec<(Solve, Span)>,

    // TODO: These aren't read yet but they will need to be as a part of semantic analysis and
    // optimisation.
    #[allow(dead_code)]
    funcs: Vec<(FnDecl, Span)>,
    #[allow(dead_code)]
    enums: Vec<EnumDecl>,
    #[allow(dead_code)]
    interfaces: Vec<InterfaceDecl>,
    #[allow(dead_code)]
    contracts: Vec<ContractDecl>,
    #[allow(dead_code)]
    externs: Vec<(Vec<FnSig>, Span)>,
}

impl IntermediateIntent {
    pub(super) fn from_ast(ast: &[ast::Decl]) -> Result<Self> {
        from_ast::from_ast(ast)
    }

    pub(super) fn compile(self) -> Result<Intent> {
        compile::compile(self)
    }
}

/// A state specification with an optional type.
struct State {
    name: Path,
    ty: Option<Type>,
    expr: Expr,
}

/// A decision variable with an optional type.
struct Var {
    name: Path,
    ty: Option<Type>,
}

/// A function (macro) to be applied and reduced where called.
// TODO: This isn't read yet but will need to be as a part of semantic analysis and optimisation.
#[allow(dead_code)]
struct FnDecl {
    sig: FnSig,
    local_vars: Vec<(Var, Span)>,
    local_constraints: Vec<(Expr, Span)>,
    returned_constraint: Expr,
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#[cfg(test)]
use crate::expr;

#[test]
fn single_let() {
    use crate::types::{PrimitiveKind::*, Type};
    let ast = vec![ast::Decl::Let {
        // `let foo: real;`
        name: ast::Ident {
            name: "foo".to_owned(),
            span: 4..7,
        },
        ty: Some(Type::Primitive {
            kind: Real,
            span: 9..13,
        }),
        init: None,
        span: 0..14,
    }];

    assert!(IntermediateIntent::from_ast(&ast).is_ok());
}

#[test]
fn double_let_clash() {
    use crate::types::{PrimitiveKind::*, Type};
    let ast = vec![
        ast::Decl::Let {
            // `let foo: real;`
            name: ast::Ident {
                name: "foo".to_owned(),
                span: 4..7,
            },
            ty: Some(Type::Primitive {
                kind: Real,
                span: 9..13,
            }),
            init: None,
            span: 0..14,
        },
        ast::Decl::Let {
            // `let foo: real;`
            name: ast::Ident {
                name: "foo".to_owned(),
                span: 19..22,
            },
            ty: Some(Type::Primitive {
                kind: Real,
                span: 24..28,
            }),
            init: None,
            span: 15..29,
        },
    ];

    // TODO compare against an error message using the spans.
    // https://github.com/essential-contributions/yurt/issues/172
    let res = IntermediateIntent::from_ast(&ast);
    assert!(res.is_err_and(|e| {
        assert_eq!(
            format!("{e:?}"),
            r#"NameClash { sym: "foo", span: 19..22, prev_span: 4..7 }"#
        );
        true
    }));
}

#[test]
fn let_fn_clash() {
    use crate::types::{PrimitiveKind::*, Type};
    let ast = vec![
        ast::Decl::Let {
            // `let bar: real;`
            name: ast::Ident {
                name: "bar".to_owned(),
                span: 4..7,
            },
            ty: Some(Type::Primitive {
                kind: Real,
                span: 9..13,
            }),
            init: None,
            span: 0..14,
        },
        ast::Decl::Fn {
            fn_sig: ast::FnSig {
                // `fn bar() -> bool { false }`
                name: ast::Ident {
                    name: "bar".to_owned(),
                    span: 18..21,
                },
                params: Vec::new(),
                return_type: Type::Primitive {
                    kind: Bool,
                    span: 27..31,
                },
                span: 15..31,
            },
            body: ast::Block {
                statements: Vec::new(),
                final_expr: Box::new(ast::Expr::Immediate {
                    value: expr::Immediate::Bool(false),
                    span: 34..39,
                }),
                span: 15..41,
            },
            span: 15..41,
        },
    ];

    // TODO ditto
    let res = IntermediateIntent::from_ast(&ast);
    assert!(res.is_err_and(|e| {
        assert_eq!(
            format!("{e:?}"),
            r#"NameClash { sym: "bar", span: 18..21, prev_span: 4..7 }"#
        );
        true
    }));
}
