use crate::{
    error::FormatterError,
    formatter::{Format, FormattedCode},
    lexer::Token,
};
use std::fmt::{self, Write};

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
    Constraint {
        constraint_token: Token<'sc>,
        expr: Expr<'sc>,
    },
    Fn {
        fn_token: Token<'sc>,
        name: String,
        fn_sig: Option<Vec<(String, Type)>>,
        return_type: Type,
        // body: Block, TODO:
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
                write!(formatted_code, "{} {}", let_token, name)?;

                if let Some((colon_token, ty)) = colon_token_and_ty {
                    write!(formatted_code, "{} {}", colon_token, ty)?;
                }

                if let Some((eq_token, init)) = eq_token_and_init {
                    write!(formatted_code, " {} ", eq_token)?;
                    init.format(formatted_code)?;
                }
            }
            Self::Solve {
                solve_token,
                directive,
                expr,
            } => {
                write!(formatted_code, "{} {}", solve_token, directive)?;

                if let Some(expr) = expr {
                    write!(formatted_code, " ")?;
                    expr.format(formatted_code)?;
                }
            }
            Self::Constraint {
                constraint_token,
                expr,
            } => {
                write!(formatted_code, "{} ", constraint_token)?;
                expr.format(formatted_code)?;
            }
            Self::Fn {
                fn_token,
                name,
                fn_sig,
                return_type,
                // body,
            } => {
                write!(formatted_code, "{} {} (", fn_token, name)?;

                if let Some(fn_sig) = fn_sig {
                    let parameters: Vec<String> = fn_sig
                        .iter()
                        .map(|(param_name, param_type)| format!("{}: {}", param_name, param_type))
                        .collect();

                    write!(formatted_code, "{}", parameters.join(", "))?;
                }

                writeln!(formatted_code, ") -> {} {{", return_type)?;
                // block stuff
                write!(formatted_code, "\n}}")?;
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Type {
    Primitive(String),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Primitive(primitive_ty) => write!(f, "{primitive_ty}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Immediate(pub String);

impl Format for Immediate {
    fn format(&self, formatted_code: &mut FormattedCode) -> Result<(), FormatterError> {
        write!(formatted_code, "{}", self.0)?;

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
            write!(formatted_code, "::")?;
        }
        write!(formatted_code, "{}", self.idents.join("::"))?;
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
            writeln!(formatted_code, "{}", Token::Semi)?;
        }

        Ok(())
    }
}
