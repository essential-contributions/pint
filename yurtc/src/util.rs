/// A place for small utility functions which can use used in various places throughout the crate.

macro_rules! write_many_iter_with_ii {
    ($f: expr, $i: ident, $sep: literal, $ii: ident) => {
        if let Some(e) = $i.next() {
            write!($f, "{}", $ii.with_ii(e))?;
        }
        for e in $i {
            write!($f, "{}{}", $sep, $ii.with_ii(e))?;
        }
    };
}

macro_rules! write_many_with_ii {
    ($f: expr, $vec: expr, $sep: literal, $ii: ident) => {
        let mut i = $vec.iter();
        crate::util::write_many_iter_with_ii!($f, i, $sep, $ii);
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

macro_rules! write_many {
    ($f: expr, $vec: expr, $sep: literal) => {
        let mut i = $vec.iter();
        crate::util::write_many_iter!($f, i, $sep);
    };
}

pub(crate) use write_many;
pub(crate) use write_many_iter;
pub(crate) use write_many_iter_with_ii;
pub(crate) use write_many_with_ii;
