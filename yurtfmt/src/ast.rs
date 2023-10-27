use crate::{
    error::FormatterError,
    formatter::{Format, FormattedCode},
};
use std::fmt::Write;

#[cfg(test)]
mod tests;

pub(super) type Ast<'sc> = Vec<Decl<'sc>>;

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Decl<'sc> {
    Use {
        use_tree: UseTree,
    },
    Value {
        name: String,
        ty: Option<Type>,
        init: Option<Expr<'sc>>,
    },
    Contract {
        name: String,
        expr: Expr<'sc>,
        paths: Option<Vec<Path>>,
        fn_sigs: Vec<FnSig>,
    },
    Solve {
        directive: String,
        expr: Option<Expr<'sc>>,
    },
    NewType {
        name: String,
        ty: Type,
    },
    Constraint {
        expr: Expr<'sc>,
    },
    Fn {
        fn_sig: FnSig,
        body: Block<'sc>,
    },
    Interface {
        name: String,
        fn_sigs: Vec<FnSig>,
    },
    Extern {
        fn_sigs: Vec<FnSig>,
    },
}

impl<'sc> Format for Decl<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        match self {
            Self::Use { use_tree } => {
                formatted_code.write("use ");
                use_tree.format(formatted_code)?;
                formatted_code.write(";");
            }
            Self::Value { name, ty, init } => {
                formatted_code.write(&format!("let {}", name));

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
            } => {
                formatted_code.write(&format!("contract {}(", name));
                expr.format(formatted_code)?;
                formatted_code.write(")");

                if let Some(paths) = paths {
                    formatted_code.write(" implements ");

                    for (i, path) in paths.iter().enumerate() {
                        path.format(formatted_code)?;

                        // If not the last element, add a comma
                        if i < paths.len() - 1 {
                            formatted_code.write(", ")
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
                    formatted_code.write_line(";")
                }

                formatted_code.decrease_indent();
                formatted_code.write_line("}");
            }
            Self::Solve { directive, expr } => {
                formatted_code.write(&format!("solve {}", directive));

                if let Some(expr) = expr {
                    formatted_code.write(" ");
                    expr.format(formatted_code)?;
                }

                formatted_code.write_line(";");
            }
            Self::NewType { name, ty } => {
                formatted_code.write(&format!("type {} = ", name));
                ty.format(formatted_code)?;
                formatted_code.write_line(";");
            }
            Self::Constraint { expr } => {
                formatted_code.write("constraint ");
                expr.format(formatted_code)?;
                formatted_code.write_line(";");
            }
            Self::Fn { fn_sig, body } => {
                fn_sig.format(formatted_code)?;

                formatted_code.write_line(" {");

                body.format(formatted_code)?;

                formatted_code.write("\n}");
            }
            Self::Interface { name, fn_sigs } => {
                formatted_code.write(&format!("interface {} {{", name));

                formatted_code.increase_indent();

                for (i, fn_sig) in fn_sigs.iter().enumerate() {
                    if i == 0 {
                        formatted_code.write_line("");
                    }

                    fn_sig.format(formatted_code)?;
                    formatted_code.write_line(";")
                }

                formatted_code.decrease_indent();

                formatted_code.write_line("}");
            }
            Self::Extern { fn_sigs } => {
                formatted_code.write_line("extern {");

                formatted_code.increase_indent();

                for (i, fn_sig) in fn_sigs.iter().enumerate() {
                    if i == 0 {
                        formatted_code.write_line("");
                    }

                    fn_sig.format(formatted_code)?;
                    formatted_code.write_line(";")
                }

                formatted_code.decrease_indent();

                formatted_code.write_line("}");
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum UseTree {
    Name(String),
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

impl Format for UseTree {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        match self {
            Self::Name(name) => {
                formatted_code.write(name);
            }
            Self::Path { prefix, suffix } => {
                formatted_code.write(&format!("{}::", prefix));
                suffix.format(formatted_code)?;
            }
            Self::Group { imports } => {
                formatted_code.write("{");
                for (i, import) in imports.iter().enumerate() {
                    import.format(formatted_code)?;

                    if i < imports.len() - 1 {
                        formatted_code.write(", ");
                    }
                }
                formatted_code.write("}");
            }
            Self::Alias { name, alias } => {
                formatted_code.write(&format!("{} as {}", name, alias));
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnSig {
    pub(super) name: String,
    pub(super) params: Option<Vec<(String, Type)>>,
    pub(super) return_type: Type,
}

impl Format for FnSig {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        formatted_code.write(&format!("fn {}(", self.name));

        if let Some(params) = &self.params {
            for (i, (param_name, param_type)) in params.iter().enumerate() {
                formatted_code.write(&format!("{}: ", param_name));
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

#[derive(Clone, Debug, PartialEq)]
pub struct Block<'sc> {
    pub(super) statements: Vec<Decl<'sc>>,
    pub(super) final_expr: Box<Expr<'sc>>,
}

impl<'sc> Format for Block<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
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

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Type {
    Primitive(String),
    Tuple(Vec<(Option<String>, Type)>),
}

impl Format for Type {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        match self {
            Type::Primitive(primitive_ty) => formatted_code.write(primitive_ty),
            Type::Tuple(tuple_ty) => {
                formatted_code.write("{ ");

                for (i, (name, ty)) in tuple_ty.iter().enumerate() {
                    if let Some(name) = name {
                        formatted_code.write(&format!("{}: ", name));
                    }

                    ty.format(formatted_code)?;

                    // If not the last element, append a comma
                    if i < tuple_ty.len() - 1 {
                        formatted_code.write(", ");
                    }
                }

                formatted_code.write(" }");
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Immediate(pub String);

impl Format for Immediate {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        formatted_code.write(&self.0);

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Path {
    pub pre_colon: bool,
    pub idents: Vec<String>,
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

#[derive(Clone, Debug, PartialEq)]
pub(super) struct UnaryOp<'sc> {
    pub prefix_op: &'sc str,
    pub expr: Box<Expr<'sc>>,
}

impl<'sc> Format for UnaryOp<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        write!(formatted_code, "{}", self.prefix_op)?;
        self.expr.format(formatted_code)?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct BinaryOp<'sc> {
    pub op: &'sc str,
    pub lhs: Box<Expr<'sc>>,
    pub rhs: Box<Expr<'sc>>,
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
pub(super) enum Expr<'sc> {
    Immediate(Immediate),
    Path(Path),
    UnaryOp(UnaryOp<'sc>),
    BinaryOp(BinaryOp<'sc>),
}

impl<'sc> Format for Expr<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        match self {
            Self::Immediate(immediate) => immediate.format(formatted_code)?,
            Self::Path(path) => path.format(formatted_code)?,
            Self::UnaryOp(unary_op) => unary_op.format(formatted_code)?,
            Self::BinaryOp(binary_op) => binary_op.format(formatted_code)?,
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
