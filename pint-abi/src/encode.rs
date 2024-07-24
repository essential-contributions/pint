use crate::{types::essential::Word, write::Write};

/// A trait for encoding types as Essential `Word`s in a manner compatible with Pint's ABI.
pub trait Encode {
    /// Encode `self` in words with the given writer.
    fn encode<W: Write>(&self, w: &mut W) -> Result<(), W::Error>;
}

impl Encode for Word {
    fn encode<W: Write>(&self, w: &mut W) -> Result<(), W::Error> {
        w.write_word(*self)
    }
}

impl Encode for bool {
    fn encode<W: Write>(&self, w: &mut W) -> Result<(), W::Error> {
        Word::from(*self).encode(w)
    }
}

impl<T: Encode> Encode for [T] {
    fn encode<W: Write>(&self, w: &mut W) -> Result<(), W::Error> {
        for elem in self {
            elem.encode(w)?;
        }
        Ok(())
    }
}

impl<T: Encode, const N: usize> Encode for [T; N] {
    fn encode<W: Write>(&self, w: &mut W) -> Result<(), W::Error> {
        self[..].encode(w)?;
        Ok(())
    }
}

macro_rules! impl_encode_for_tuple {
    ($($T:ident),+) => {
        impl<$($T: Encode),+> Encode for ($($T),+) {
            fn encode<W: Write>(&self, w: &mut W) -> Result<(), W::Error> {
                #[allow(non_snake_case)]
                let ($($T),+) = self;
                $(
                    $T.encode(w)?;
                )+
                Ok(())
            }
        }
    };
}

impl_encode_for_tuple!(A, B);
impl_encode_for_tuple!(A, B, C);
impl_encode_for_tuple!(A, B, C, D);
impl_encode_for_tuple!(A, B, C, D, E);
impl_encode_for_tuple!(A, B, C, D, E, F);
impl_encode_for_tuple!(A, B, C, D, E, F, G);
impl_encode_for_tuple!(A, B, C, D, E, F, G, H);
impl_encode_for_tuple!(A, B, C, D, E, F, G, H, I);
impl_encode_for_tuple!(A, B, C, D, E, F, G, H, I, J);
impl_encode_for_tuple!(A, B, C, D, E, F, G, H, I, J, K);
impl_encode_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L);
