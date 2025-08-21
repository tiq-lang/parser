//! This module contains `ByteStreamView` struct, which provides a way to create a 'detached' view
//! over the stream, that is easier to work with than with raw offsets.

use super::core::{ByteStream, ByteStreamError, Position, checked_offset};
use std::num::NonZero;

/// Byte stream over another stream, that has some positive offset from it.
#[derive(Clone, Copy)]
pub struct ByteStreamView<'a, S: ByteStream + ?Sized + 'a> {
    stream: &'a S,
    offset: usize,
}

impl<'a, S: ByteStream + ?Sized + 'a> ByteStreamView<'a, S> {
    pub fn new(stream: &'a S) -> Self {
        Self { stream, offset: 0 }
    }

    pub fn get_offset(&self) -> usize {
        self.offset
    }

    /// 'Instantiation' of `checked_offset` for `ByteStreamView`
    fn checked_offset(&self, offset: usize) -> Result<usize, S::Error> {
        checked_offset::<Self>(self.offset, offset)
    }
}

impl<'a, S: ByteStream + ?Sized + 'a> ByteStream for ByteStreamView<'a, S> {
    type Error = S::Error;

    unsafe fn advance_by(&mut self, amount: NonZero<usize>) -> Result<(), Self::Error> {
        let new_offset = self.checked_offset(amount.get())?;
        self.offset = new_offset;
        Ok(())
    }

    unsafe fn peek_offset(&self, offset: usize) -> Result<Option<u8>, Self::Error> {
        // Safe if `advance_by` invariant is upheld
        unsafe { self.stream.peek_offset(self.checked_offset(offset)?) }
    }

    unsafe fn get_slice_offset(
        &self,
        offset: usize,
        length: NonZero<usize>,
    ) -> Result<&[u8], Self::Error> {
        // Safe because of function precondition
        unsafe {
            self.stream
                .get_slice_offset(self.checked_offset(offset)?, length)
        }
    }

    fn get_position(&self) -> Result<Position, Self::Error> {
        self.stream
            .get_position()?
            .offset(
                self.offset
                    .try_into()
                    .map_err(|_| Self::Error::position_overflow_error())?,
            )
            .ok_or_else(Self::Error::position_overflow_error)
    }
}
