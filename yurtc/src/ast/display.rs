use crate::util::write_many;

use std::fmt::{Display, Formatter, Result};

impl Display for super::Ast {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write_many!(f, self.0, "; ");
        write!(f, ";")
    }
}

impl Display for super::Decl {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            super::Decl::Use {
                is_absolute,
                use_tree,
                ..
            } => {
                write!(f, "use ")?;
                if *is_absolute {
                    write!(f, "::")?;
                }
                write!(f, "{use_tree}")
            }

            super::Decl::Let { name, ty, init, .. } => {
                write!(f, "let {name}")?;
                if let Some(ty) = ty {
                    write!(f, ": {ty}")?;
                }
                if let Some(expr) = init {
                    write!(f, " = {expr}")?;
                }
                Ok(())
            }

            super::Decl::State { name, ty, init, .. } => {
                write!(f, "state {name}")?;
                if let Some(ty) = ty {
                    write!(f, ": {ty}")?;
                }
                write!(f, "= {init}")
            }

            super::Decl::Constraint { expr, .. } => write!(f, "constraint {expr}"),

            super::Decl::Fn { fn_sig, body, .. } => write!(f, "{fn_sig} {body}"),

            super::Decl::Solve { directive, .. } => {
                write!(f, "solve ")?;
                match directive {
                    super::SolveFunc::Satisfy => write!(f, "satisfy"),
                    super::SolveFunc::Minimize(e) => write!(f, "minimize {e}"),
                    super::SolveFunc::Maximize(e) => write!(f, "maximize {e}"),
                }
            }

            super::Decl::Enum(decl) => decl.fmt(f),

            super::Decl::Interface(decl) => decl.fmt(f),

            super::Decl::Contract(decl) => decl.fmt(f),

            super::Decl::NewType { name, ty, .. } => write!(f, "type {name} = {ty}"),

            super::Decl::Extern { functions, .. } => {
                write!(f, "extern {{ ")?;
                for function in functions {
                    write!(f, "{function}; ")?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl Display for super::UseTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Glob(_) => write!(f, "*"),
            Self::Name { name, .. } => name.fmt(f),
            Self::Path { prefix, suffix, .. } => write!(f, "{prefix}::{suffix}"),
            Self::Group { imports, .. } => {
                write!(f, "{{")?;
                write_many!(f, imports, ", ");
                write!(f, "}}")
            }
            Self::Alias { name, alias, .. } => write!(f, "{name} as {alias}"),
        }
    }
}

impl Display for super::Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.is_absolute {
            write!(f, "::")?;
        }
        write_many!(f, self.path, "::");
        Ok(())
    }
}

impl Display for super::Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.name)
    }
}

impl Display for super::Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{{ ")?;
        for decl in &self.statements {
            write!(f, "{}; ", decl)?;
        }
        write!(f, "{} }}", self.final_expr)
    }
}
