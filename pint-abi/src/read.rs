use crate::types::essential::Word;

/// Allows for reading words from a source, largely inspired by [`std::io::Read`].
pub trait Read {
    /// A type representing any error that might occur while reading words.
    type Error: core::fmt::Debug + core::fmt::Display;

    /// Read words from this source into the specified buffer.
    ///
    /// Returns how many words were read.
    ///
    /// Repeated calls to `read` should use the same cursor, i.e. the same words
    /// should not be yielded twice.
    fn read(&mut self, buf: &mut [Word]) -> Result<usize, Self::Error>;
}

impl<T: Read> Read for &'_ mut T {
    type Error = T::Error;
    fn read(&mut self, buf: &mut [Word]) -> Result<usize, Self::Error> {
        (*self).read(buf)
    }
}

impl Read for &'_ [Word] {
    type Error = core::convert::Infallible;
    fn read(&mut self, buf: &mut [Word]) -> Result<usize, Self::Error> {
        let amt = core::cmp::min(self.len(), buf.len());
        let (a, b) = self.split_at(amt);
        buf[..amt].copy_from_slice(a);
        *self = b;
        Ok(amt)
    }
}
