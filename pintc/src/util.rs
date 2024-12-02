//! A place for small utility functions which can use used in various places throughout the crate.

macro_rules! write_many_iter_with_ctrct {
    ($f: expr, $i: ident, $sep: literal, $contract: ident) => {
        if let Some(e) = $i.next() {
            write!($f, "{}", $contract.with_ctrct(e))?;
        }
        for e in $i {
            write!($f, "{}{}", $sep, $contract.with_ctrct(e))?;
        }
    };
}

macro_rules! write_many_with_ctrct {
    ($f: expr, $vec: expr, $sep: literal, $contract: ident) => {
        let mut i = $vec.iter();
        crate::util::write_many_iter_with_ctrct!($f, i, $sep, $contract);
    };
}

macro_rules! write_many_iter {
    ($f: expr, $i: ident, $sep: literal) => {
        if let Some(e) = $i.next() {
            write!($f, "{e}",)?;
        }
        for e in $i {
            write!($f, "{}{e}", $sep)?;
        }
    };
}

pub(crate) use write_many_iter;
pub(crate) use write_many_iter_with_ctrct;
pub(crate) use write_many_with_ctrct;
