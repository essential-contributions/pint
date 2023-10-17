use crate::{
    error::FormatterError,
    formatter::{Format, FormattedCode},
    lexer::Token,
};
use std::fmt::Write;

#[cfg(test)]
mod tests;

pub(super) type Ast<'sc> = Vec<Decl<'sc>>;

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Decl<'sc> {
    Value {
        let_token: Token<'sc>,
        name: String,
        colon_token_and_ty: Option<(Token<'sc>, Type)>,
        eq_token_and_init: Option<(Token<'sc>, Expr<'sc>)>,
    },
    Solve {
        solve_token: Token<'sc>,
        directive: String,
        expr: Option<Expr<'sc>>,
    },
    NewType {
        type_token: Token<'sc>,
        name: String,
        ty: Type,
    },
    Constraint {
        constraint_token: Token<'sc>,
        expr: Expr<'sc>,
    },
    Fn {
        fn_token: Token<'sc>,
        name: String,
        fn_sig: Option<Vec<(String, Type)>>,
        return_type: Type,
        body: Block<'sc>,
    },
}

impl<'sc> Format for Decl<'sc> {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        match self {
            Self::Value {
                let_token,
                name,
                colon_token_and_ty,
                eq_token_and_init,
            } => {
                formatted_code.write(&format!("{} {}", let_token, name));

                if let Some((colon_token, ty)) = colon_token_and_ty {
                    formatted_code.write(&format!("{} ", colon_token));
                    ty.format(formatted_code)?;
                }

                if let Some((eq_token, init)) = eq_token_and_init {
                    formatted_code.write(&format!(" {} ", eq_token));
                    init.format(formatted_code)?;
                }

                formatted_code.write_line(&format!("{}", Token::Semi));
            }
            Self::Solve {
                solve_token,
                directive,
                expr,
            } => {
                formatted_code.write(&format!("{} {}", solve_token, directive));

                if let Some(expr) = expr {
                    formatted_code.write(" ");
                    expr.format(formatted_code)?;
                }

                formatted_code.write_line(&format!("{}", Token::Semi));
            }
            Self::NewType {
                type_token,
                name,
                ty,
            } => {
                formatted_code.write(&format!("{} {} = ", type_token, name));
                ty.format(formatted_code)?;
                formatted_code.write(&format!("{}", Token::Semi));
            }
            Self::Constraint {
                constraint_token,
                expr,
            } => {
                formatted_code.write(&format!("{} ", constraint_token));
                expr.format(formatted_code)?;
                formatted_code.write_line(&format!("{}", Token::Semi));
            }
            Self::Fn {
                fn_token,
                name,
                fn_sig,
                return_type,
                body,
            } => {
                formatted_code.write(&format!("{} {} (", fn_token, name));

                if let Some(fn_sig) = fn_sig {
                    for (i, (param_name, param_type)) in fn_sig.iter().enumerate() {
                        formatted_code.write(&format!("{}: ", param_name));
                        param_type.format(formatted_code)?;

                        // If not the last element, add a comma
                        if i < fn_sig.len() - 1 {
                            formatted_code.write(", ");
                        }
                    }
                }

                formatted_code.write(") -> ");
                return_type.format(formatted_code)?;
                formatted_code.write_line(" {");

                body.format(formatted_code)?;

                formatted_code.write("\n}");
            }
        }

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

                    // Instead of using the format! macro, directly format the Type.
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
    pub prefix_op: Token<'sc>,
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
    pub op: Token<'sc>,
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
