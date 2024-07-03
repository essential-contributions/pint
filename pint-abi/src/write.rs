use essential_types::Word;

/// A trait for writing essential `Word`s into buffers or streams.
pub trait Write {
    /// A type representing any error that might occur while writing words.
    type Error: core::fmt::Debug + core::fmt::Display;
    /// Write a single word into this writer, returning whether the write succeeded.
    fn write_word(&mut self, w: Word) -> Result<(), Self::Error>;
}

impl<'a, W: Write> Write for &'a mut W {
    type Error = W::Error;
    fn write_word(&mut self, w: Word) -> Result<(), Self::Error> {
        (*self).write_word(w)
    }
}

impl Write for Vec<Word> {
    type Error = core::convert::Infallible;
    fn write_word(&mut self, w: Word) -> Result<(), Self::Error> {
        self.push(w);
        Ok(())
    }
}
