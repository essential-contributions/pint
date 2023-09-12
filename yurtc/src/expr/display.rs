use std::fmt::{Display, Formatter, Result};

impl<Path, BlockExpr> Display for super::Expr<Path, BlockExpr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        todo!()
    }
}

impl Display for super::Immediate {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        todo!()
    }
}
