use crate::{
    read::Read,
    types::essential::{convert::bool_from_word, Word},
};
use core::fmt::{Debug, Display};
use thiserror::Error;

/// A trait for decoding types from Essential `Word`s in a manner compatible with Pint's ABI.
pub trait Decode: Sized {
    /// Any error that might occur during decoding.
    type Error: Error;
    /// Decode an instance of `Self` from words read from the given reader.
    fn decode<R: Read>(r: &mut R) -> Result<Self, Self::Error>;
}

/// Requirements for the `Error` type in a `Decode` implementation.
pub trait Error: Debug + Display {}

/// Construct a decode `Error` from a `Read` implementation error.
pub trait FromReadError {
    /// Forward an error produced by the `Read`er.
    fn from_read_error<E: Debug + Display>(err: E) -> Self;
}

/// An error occurring during decoding where the `Read` implementation fails to
/// read enough words.
#[derive(Debug, Error)]
#[error("failed to read enough bytes: expected `{expected}`, found `{found}`")]
pub struct NotEnoughWords {
    /// The expected number of words.
    pub expected: usize,
    /// The actual number of words read.
    pub found: usize,
}

/// The `Read` implementation error formatted into a `String` using the `Display` impl.
#[derive(Debug, Error)]
#[error("reader error: {0}")]
pub struct ReadError(String);

/// A general-use error for types with a known size in words.
#[derive(Debug, Error)]
pub enum SizedError {
    /// The `Read` implementation returned an error.
    #[error("{0}")]
    Reader(#[from] ReadError),
    /// Failed to read enough bytes from the reader.
    #[error("{0}")]
    NotEnoughWords(#[from] NotEnoughWords),
}

/// The read `Word` was invalid.
#[derive(Debug, Error)]
#[error("`bool` expected `0` or `1`, found `{0}`")]
pub struct BoolInvalid(pub Word);

/// An error occurring when decoding a `bool`.
#[derive(Debug, Error)]
pub enum BoolError {
    /// Failed to read the word.
    #[error("{0}")]
    Sized(#[from] SizedError),
    /// The read `Word` was invalid.
    #[error("{0}")]
    Invalid(#[from] BoolInvalid),
}

/// Failed to decode a pair in the form of a tuple.
#[derive(Debug, Error)]
pub enum PairError<A, B> {
    #[error("{0}")]
    Left(A),
    #[error("{0}")]
    Right(B),
}

impl<T: Debug + Display> Error for T {}

impl FromReadError for ReadError {
    fn from_read_error<E: Debug + Display>(err: E) -> Self {
        ReadError(format!("{err}"))
    }
}

impl FromReadError for SizedError {
    fn from_read_error<E: Debug + Display>(err: E) -> Self {
        Self::Reader(ReadError::from_read_error(err))
    }
}

impl FromReadError for BoolError {
    fn from_read_error<E: Debug + Display>(err: E) -> Self {
        Self::Sized(SizedError::from_read_error(err))
    }
}

impl Decode for Word {
    type Error = SizedError;
    fn decode<R: Read>(r: &mut R) -> Result<Self, Self::Error> {
        let [w] = read_exact(r)?;
        Ok(w)
    }
}

impl Decode for bool {
    type Error = BoolError;
    fn decode<R: Read>(r: &mut R) -> Result<Self, Self::Error> {
        let [w] = read_exact(r)?;
        bool_from_word(w).ok_or(BoolInvalid(w)).map_err(From::from)
    }
}

impl<T> Decode for Option<T>
where
    T: Decode,
{
    type Error = PairError<T::Error, SizedError>;

    fn decode<R: Read>(r: &mut R) -> Result<Self, Self::Error> {
        T::decode(r) // Decode the inner value of Option<T>
            .map_err(PairError::Left)
            .and_then(|val| {
                read_exact(r) // Read the tag
                    .map_err(PairError::Right)
                    .map(|[tag]| if tag == 0 { None } else { Some(val) }) // Match the tag
            })
    }
}

impl<const N: usize, T> Decode for [T; N]
where
    T: Decode,
{
    type Error = T::Error;
    fn decode<R: Read>(r: &mut R) -> Result<Self, Self::Error> {
        // TODO: Use `core::array::try_from_fn` once stabilized rather than `Vec`.
        let mut vec = Vec::with_capacity(N);
        for _ in 0..N {
            vec.push(T::decode(r)?);
        }
        let mut elems = vec.into_iter();
        Ok(core::array::from_fn(|_| {
            elems.next().expect("Vec gauranteed to have len `N`")
        }))
    }
}

/// A macro for implementing `Decode` for non-pair tuples.
macro_rules! impl_decode_for_tuple {
    (A, $($T:ident),+) => {
        #[allow(unused_parens)]
        impl<A: Decode, $($T: Decode),+> Decode for (A, $($T),+) {
            type Error = PairError<A::Error, <($($T),+) as Decode>::Error>;
            fn decode<R: Read>(r: &mut R) -> Result<Self, Self::Error> {
                let a: A = <_>::decode(r).map_err(PairError::Left)?;
                #[allow(non_snake_case)]
                let ($($T),+): ($($T),+) = <_>::decode(r).map_err(PairError::Right)?;
                Ok((a, $($T),+))
            }
        }
    };
}

impl_decode_for_tuple!(A, B);
impl_decode_for_tuple!(A, B, C);
impl_decode_for_tuple!(A, B, C, D);
impl_decode_for_tuple!(A, B, C, D, E);
impl_decode_for_tuple!(A, B, C, D, E, F);
impl_decode_for_tuple!(A, B, C, D, E, F, G);
impl_decode_for_tuple!(A, B, C, D, E, F, G, H);
impl_decode_for_tuple!(A, B, C, D, E, F, G, H, I);
impl_decode_for_tuple!(A, B, C, D, E, F, G, H, I, J);
impl_decode_for_tuple!(A, B, C, D, E, F, G, H, I, J, K);
impl_decode_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L);

fn read_exact<const N: usize, R: Read>(r: &mut R) -> Result<[Word; N], SizedError> {
    let mut arr = [0; N];
    let amt = r.read(&mut arr).map_err(SizedError::from_read_error)?;
    if amt != N {
        let err = NotEnoughWords {
            expected: N,
            found: amt,
        };
        return Err(err.into());
    }
    Ok(arr)
}
