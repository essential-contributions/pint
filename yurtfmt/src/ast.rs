use crate::{
    error::FormatterError,
    formatter::{Format, FormattedCode},
    span::{Span, Spanned},
};
use std::fmt::Write;

#[cfg(test)]
mod tests;

pub(super) type Ast<'sc> = Vec<Decl<'sc>>;

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Decl<'sc> {
    Use {
        use_tree: UseTree,
        span: Span,
    },
    Value {
        name: String,
        ty: Option<Type<'sc>>,
        init: Option<Expr<'sc>>,
        span: Span,
    },
    Contract {
        name: String,
        expr: Expr<'sc>,
        paths: Option<Vec<Path>>,
        fn_sigs: Vec<FnSig<'sc>>,
        span: Span,
    },
    Solve {
        directive: String,
        expr: Option<Expr<'sc>>,
        span: Span,
    },
    NewType {
        name: String,
        ty: Type<'sc>,
        span: Span,
    },
    Constraint {
        expr: Expr<'sc>,
        span: Span,
    },
    Fn {
        fn_sig: FnSig<'sc>,
        body: Block<'sc>,
        span: Span,
    },
    Interface {
        name: String,
        fn_sigs: Vec<FnSig<'sc>>,
        span: Span,
    },
    State {
        name: String,
        ty: Option<Type<'sc>>,
        expr: Expr<'sc>,
        span: Span,
    },
    Extern {
        fn_sigs: Vec<FnSig<'sc>>,
        span: Span,
    },
    Enum {
        name: String,
        variants: Vec<String>,
        span: Span,
    },
}

impl<'sc> Format for Decl<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        match self {
            Self::Use { use_tree, .. } => {
                formatted_code.write("use ");
                use_tree.format(formatted_code)?;
                formatted_code.write(";");
            }
            Self::Value { name, ty, init, .. } => {
                formatted_code.write(&format!("let {name}"));

                if let Some(ty) = ty {
                    formatted_code.write(": ");
                    ty.format(formatted_code)?;
                }

                if let Some(init) = init {
                    formatted_code.write(" = ");
                    init.format(formatted_code)?;
                }

                formatted_code.write_line(";");
            }
            Self::Contract {
                name,
                expr,
                paths,
                fn_sigs,
                ..
            } => {
                formatted_code.write(&format!("contract {name}("));
                expr.format(formatted_code)?;
                formatted_code.write(")");

                if let Some(paths) = paths {
                    formatted_code.write(" implements ");

                    for (i, path) in paths.iter().enumerate() {
                        path.format(formatted_code)?;

                        // If not the last element, add a comma
                        if i < paths.len() - 1 {
                            formatted_code.write(", ");
                        }
                    }
                }

                formatted_code.write(" {");
                formatted_code.increase_indent();

                for (i, fn_sig) in fn_sigs.iter().enumerate() {
                    if i == 0 {
                        formatted_code.write_line("");
                    }

                    fn_sig.format(formatted_code)?;
                    formatted_code.write_line(";");
                }

                formatted_code.decrease_indent();
                formatted_code.write_line("}");
            }
            Self::Solve {
                directive, expr, ..
            } => {
                formatted_code.write(&format!("solve {directive}"));

                if let Some(expr) = expr {
                    formatted_code.write(" ");
                    expr.format(formatted_code)?;
                }

                formatted_code.write_line(";");
            }
            Self::NewType { name, ty, .. } => {
                formatted_code.write(&format!("type {name} = "));
                ty.format(formatted_code)?;
                formatted_code.write_line(";");
            }
            Self::Constraint { expr, .. } => {
                formatted_code.write("constraint ");
                expr.format(formatted_code)?;
                formatted_code.write_line(";");
            }
            Self::Fn { fn_sig, body, .. } => {
                fn_sig.format(formatted_code)?;
                formatted_code.write(" ");
                body.format(formatted_code)?;
            }
            Self::Interface { name, fn_sigs, .. } => {
                formatted_code.write(&format!("interface {name} {{"));

                formatted_code.increase_indent();

                for (i, fn_sig) in fn_sigs.iter().enumerate() {
                    if i == 0 {
                        formatted_code.write_line("");
                    }

                    fn_sig.format(formatted_code)?;
                    formatted_code.write_line(";");
                }

                formatted_code.decrease_indent();

                formatted_code.write_line("}");
            }
            Self::State { name, ty, expr, .. } => {
                formatted_code.write(&format!("state {name}"));

                if let Some(ty) = ty {
                    formatted_code.write(": ");
                    ty.format(formatted_code)?;
                }

                formatted_code.write(" = ");
                expr.format(formatted_code)?;
                formatted_code.write_line(";");
            }
            Self::Extern { fn_sigs, .. } => {
                formatted_code.write("extern {");

                formatted_code.increase_indent();

                for (i, fn_sig) in fn_sigs.iter().enumerate() {
                    if i == 0 {
                        formatted_code.write_line("");
                    }

                    fn_sig.format(formatted_code)?;
                    formatted_code.write_line(";");
                }

                formatted_code.decrease_indent();

                formatted_code.write_line("}");
            }
            Self::Enum { name, variants, .. } => {
                formatted_code.write(&format!("enum {name} = {};", &variants.join(" | ")));
            }
        }

        Ok(())
    }
}

impl<'sc> Spanned for Decl<'sc> {
    fn span(&self) -> &Span {
        use Decl::*;
        match &self {
            Use { span, .. }
            | Value { span, .. }
            | State { span, .. }
            | Constraint { span, .. }
            | Fn { span, .. }
            | Solve { span, .. }
            | Extern { span, .. }
            | Enum { span, .. }
            | Interface { span, .. }
            | Contract { span, .. }
            | NewType { span, .. } => span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum UseTree {
    Name(String, Span),
    Path {
        prefix: String,
        suffix: Box<UseTree>,
        span: Span,
    },
    Group {
        imports: Vec<UseTree>,
        span: Span,
    },
    Alias {
        name: String,
        alias: String,
        span: Span,
    },
}

impl Format for UseTree {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        match self {
            Self::Name(name, _) => {
                formatted_code.write(name);
            }
            Self::Path { prefix, suffix, .. } => {
                formatted_code.write(&format!("{prefix}::"));
                suffix.format(formatted_code)?;
            }
            Self::Group { imports, .. } => {
                formatted_code.write("{");
                for (i, import) in imports.iter().enumerate() {
                    import.format(formatted_code)?;

                    if i < imports.len() - 1 {
                        formatted_code.write(", ");
                    }
                }
                formatted_code.write("}");
            }
            Self::Alias { name, alias, .. } => {
                formatted_code.write(&format!("{name} as {alias}"));
            }
        }

        Ok(())
    }
}

impl Spanned for UseTree {
    fn span(&self) -> &Span {
        use UseTree::*;
        match &self {
            Path { span, .. } | Group { span, .. } | Alias { span, .. } => span,
            Name(_, span) => span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnSig<'sc> {
    pub(super) name: String,
    pub(super) params: Option<Vec<(String, Type<'sc>)>>,
    pub(super) return_type: Type<'sc>,
    pub(super) span: Span,
}

impl<'sc> Format for FnSig<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        formatted_code.write(&format!("fn {}(", self.name));

        if let Some(params) = &self.params {
            for (i, (param_name, param_type)) in params.iter().enumerate() {
                formatted_code.write(&format!("{param_name}: "));
                param_type.format(formatted_code)?;

                // If not the last element, add a comma
                if i < params.len() - 1 {
                    formatted_code.write(", ");
                }
            }
        }

        formatted_code.write(") -> ");
        self.return_type.format(formatted_code)?;

        Ok(())
    }
}

impl<'sc> Spanned for FnSig<'sc> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block<'sc> {
    pub(super) statements: Vec<Decl<'sc>>,
    pub(super) final_expr: Box<Expr<'sc>>,
    pub(super) span: Span,
}

impl<'sc> Format for Block<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        formatted_code.write_line("{");
        formatted_code.increase_indent();

        for (i, statement) in self.statements.iter().enumerate() {
            statement.format(formatted_code)?;

            // If not the last element, add a newline
            if i < self.statements.len() - 1 {
                formatted_code.write_line("");
            }
        }

        self.final_expr.format(formatted_code)?;

        formatted_code.decrease_indent();

        formatted_code.write_line("");
        formatted_code.write("}");

        Ok(())
    }
}

impl<'sc> Spanned for Block<'sc> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Type<'sc> {
    Primitive(String, Span),
    Tuple(Vec<(Option<String>, Self)>, Span),
    Array((Box<Self>, Vec<Expr<'sc>>), Span),
}

impl<'sc> Format for Type<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        match self {
            Type::Primitive(primitive_ty, _) => formatted_code.write(primitive_ty),
            Type::Tuple(tuple_ty, _) => {
                formatted_code.write("{ ");

                for (i, (name, ty)) in tuple_ty.iter().enumerate() {
                    if let Some(name) = name {
                        formatted_code.write(&format!("{name}: "));
                    }

                    ty.format(formatted_code)?;

                    // If not the last element, append a comma
                    if i < tuple_ty.len() - 1 {
                        formatted_code.write(", ");
                    }
                }

                formatted_code.write(" }");
            }
            Type::Array((array_ty, array_exprs), _) => {
                array_ty.format(formatted_code)?;

                for expr in array_exprs {
                    formatted_code.write("[");
                    expr.format(formatted_code)?;
                    formatted_code.write("]");
                }
            }
        }
        Ok(())
    }
}

impl<'sc> Spanned for Type<'sc> {
    fn span(&self) -> &Span {
        use Type::*;
        match &self {
            Primitive(_, span) | Tuple(_, span) | Array(_, span) => span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Immediate(pub String, pub Span);

impl Format for Immediate {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        formatted_code.write(&self.0);

        Ok(())
    }
}

impl Spanned for Immediate {
    fn span(&self) -> &Span {
        &self.1
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Path {
    pub pre_colon: bool,
    pub idents: Vec<String>,
    pub span: Span,
}

impl Format for Path {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        if self.pre_colon {
            formatted_code.write("::");
        }

        formatted_code.write(&self.idents.join("::"));
        Ok(())
    }
}

impl Spanned for Path {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct UnaryOp<'sc> {
    pub prefix_op: &'sc str,
    pub expr: Box<Expr<'sc>>,
    pub span: Span,
}

impl<'sc> Format for UnaryOp<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        write!(formatted_code, "{}", self.prefix_op)?;
        self.expr.format(formatted_code)?;
        Ok(())
    }
}

impl<'sc> Spanned for UnaryOp<'sc> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct BinaryOp<'sc> {
    pub op: &'sc str,
    pub lhs: Box<Expr<'sc>>,
    pub rhs: Box<Expr<'sc>>,
    pub span: Span,
}

impl<'sc> Format for BinaryOp<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        self.lhs.format(formatted_code)?;
        write!(formatted_code, " {} ", self.op)?;
        self.rhs.format(formatted_code)?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Call<'sc> {
    pub path: Path,
    pub args: Vec<Expr<'sc>>,
    pub span: Span,
}

impl<'sc> Format for Call<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        self.path.format(formatted_code)?;

        formatted_code.write("(");
        for (i, arg) in self.args.iter().enumerate() {
            arg.format(formatted_code)?;

            // If not the last element, append a comma
            if i < self.args.len() - 1 {
                formatted_code.write(", ");
            }
        }
        formatted_code.write(")");
        Ok(())
    }
}

impl<'sc> Spanned for Call<'sc> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct In<'sc> {
    pub lhs: Box<Expr<'sc>>,
    pub rhs: Box<Expr<'sc>>,
    pub span: Span,
}

impl<'sc> Format for In<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        self.lhs.format(formatted_code)?;
        formatted_code.write(" in ");
        self.rhs.format(formatted_code)?;
        Ok(())
    }
}

impl<'sc> Spanned for In<'sc> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Range<'sc> {
    pub lb: Box<Expr<'sc>>,
    pub ub: Box<Expr<'sc>>,
    pub span: Span,
}

impl<'sc> Format for Range<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        self.lb.format(formatted_code)?;
        formatted_code.write("..");
        self.ub.format(formatted_code)?;
        Ok(())
    }
}

impl<'sc> Spanned for Range<'sc> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Cast<'sc> {
    pub value: Box<Expr<'sc>>,
    pub ty: Type<'sc>,
    pub span: Span,
}

impl<'sc> Format for Cast<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        self.value.format(formatted_code)?;
        formatted_code.write(" as ");
        self.ty.format(formatted_code)?;

        Ok(())
    }
}

impl<'sc> Spanned for Cast<'sc> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct If<'sc> {
    pub condition: Box<Expr<'sc>>,
    pub true_code_block: Block<'sc>,
    pub false_code_block: Block<'sc>,
    pub span: Span,
}

impl<'sc> Format for If<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        formatted_code.write("if ");
        self.condition.format(formatted_code)?;
        formatted_code.write(" ");
        self.true_code_block.format(formatted_code)?;

        formatted_code.write(" else ");
        self.false_code_block.format(formatted_code)?;

        Ok(())
    }
}

impl<'sc> Spanned for If<'sc> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Cond<'sc> {
    pub cond_branches: Vec<(Expr<'sc>, Expr<'sc>)>,
    pub else_branch: Box<Expr<'sc>>,
    pub span: Span,
}

impl<'sc> Format for Cond<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        formatted_code.write_line("cond {");
        formatted_code.increase_indent();

        for cond in &self.cond_branches {
            cond.0.format(formatted_code)?;
            formatted_code.write(" => ");
            cond.1.format(formatted_code)?;

            formatted_code.write_line(",");
        }

        formatted_code.write("else => ");
        self.else_branch.format(formatted_code)?;

        formatted_code.write(",");

        formatted_code.decrease_indent();

        formatted_code.write_line("");
        formatted_code.write("}");

        Ok(())
    }
}

impl<'sc> Spanned for Cond<'sc> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct TupleExpr<'sc> {
    pub fields: Vec<(Option<String>, Expr<'sc>)>,
    pub span: Span,
}

impl<'sc> Format for TupleExpr<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        formatted_code.write("{");

        for (i, field) in self.fields.iter().enumerate() {
            formatted_code.write(" ");

            if let Some(ident) = &field.0 {
                formatted_code.write(&format!("{ident}: "))
            }

            field.1.format(formatted_code)?;

            // If not the last element, add a comma
            if i < self.fields.len() - 1 {
                formatted_code.write(",");
            } else if field.0.is_none() && self.fields.len() == 1 {
                formatted_code.write(", ");
            } else {
                formatted_code.write(" ");
            }
        }

        formatted_code.write("}");

        Ok(())
    }
}

impl<'sc> Spanned for TupleExpr<'sc> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct TupleFieldAccess<'sc> {
    pub tuple: Box<Expr<'sc>>,
    pub field: String,
    pub span: Span,
}

impl<'sc> Format for TupleFieldAccess<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        self.tuple.format(formatted_code)?;
        formatted_code.write(&format!(".{}", self.field));
        Ok(())
    }
}

impl<'sc> Spanned for TupleFieldAccess<'sc> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct ArrayExpr<'sc> {
    pub elements: Vec<Expr<'sc>>,
    pub span: Span,
}

impl<'sc> Format for ArrayExpr<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        formatted_code.write("[");

        for (i, element) in self.elements.iter().enumerate() {
            element.format(formatted_code)?;

            // If not the last element, add a comma
            if i < self.elements.len() - 1 {
                formatted_code.write(", ");
            }
        }
        formatted_code.write("]");
        Ok(())
    }
}

impl<'sc> Spanned for ArrayExpr<'sc> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct ArrayElementAccess<'sc> {
    pub array: Box<Expr<'sc>>,
    pub index: Box<Expr<'sc>>,
    pub span: Span,
}

impl<'sc> Format for ArrayElementAccess<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        self.array.format(formatted_code)?;
        formatted_code.write("[");
        self.index.format(formatted_code)?;
        formatted_code.write("]");
        Ok(())
    }
}

impl<'sc> Spanned for ArrayElementAccess<'sc> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Expr<'sc> {
    Immediate(Immediate),
    Path(Path),
    UnaryOp(UnaryOp<'sc>),
    BinaryOp(BinaryOp<'sc>),
    Call(Call<'sc>),
    In(In<'sc>),
    Range(Range<'sc>),
    Cast(Cast<'sc>),
    If(If<'sc>),
    Cond(Cond<'sc>),
    Block(Block<'sc>),
    Tuple(TupleExpr<'sc>),
    TupleFieldAccess(TupleFieldAccess<'sc>),
    Array(ArrayExpr<'sc>),
    ArrayElementAccess(ArrayElementAccess<'sc>),
}

impl<'sc> Format for Expr<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        match self {
            Self::Immediate(immediate) => immediate.format(formatted_code)?,
            Self::Path(path) => path.format(formatted_code)?,
            Self::UnaryOp(unary_op) => unary_op.format(formatted_code)?,
            Self::BinaryOp(binary_op) => binary_op.format(formatted_code)?,
            Self::Call(call) => call.format(formatted_code)?,
            Self::In(in_) => in_.format(formatted_code)?,
            Self::Range(range) => range.format(formatted_code)?,
            Self::Cast(cast) => cast.format(formatted_code)?,
            Self::If(if_) => if_.format(formatted_code)?,
            Self::Cond(cond) => cond.format(formatted_code)?,
            Self::Block(block) => block.format(formatted_code)?,
            Self::Tuple(tuple) => tuple.format(formatted_code)?,
            Self::TupleFieldAccess(tuple_field_access) => {
                tuple_field_access.format(formatted_code)?
            }
            Self::Array(array) => array.format(formatted_code)?,
            Self::ArrayElementAccess(array_field_access) => {
                array_field_access.format(formatted_code)?
            }
        }

        Ok(())
    }
}

impl<'sc> Format for Ast<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        for node in self {
            node.format(formatted_code)?;
        }

        Ok(())
    }
}
