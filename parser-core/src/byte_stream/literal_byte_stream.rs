//! This module contains the `LiteralByteStream` struct, which provides a way to create a byte
//! stream over fixed slice of bytes.

use super::core::{ByteStream, ByteStreamError, Position, checked_offset};
use std::num::NonZero;

/// Byte stream over fixed slice of bytes. Implementation of `LiteralByteStream` guarantees that
/// all of the methods of `ByteStream` trait are safe.
#[derive(Debug)]
pub struct LiteralByteStream {
    bytes: Box<[u8]>,
    position: usize,
}

impl LiteralByteStream {
    /// Creates new `LiteralByteStream` from provided `bytes`.
    pub fn new(bytes: Box<[u8]>) -> Self {
        Self { bytes, position: 0 }
    }

    /// 'Instantiation' of `checked_offset` for `LiteralByteStream`.
    fn checked_offset(&self, offset: usize) -> Result<usize, LiteralByteStreamError> {
        checked_offset::<Self>(self.position, offset)
    }
}

impl From<&[u8]> for LiteralByteStream {
    fn from(value: &[u8]) -> Self {
        Self::new(value.into())
    }
}

impl<const N: usize> From<&[u8; N]> for LiteralByteStream {
    fn from(value: &[u8; N]) -> Self {
        Self::new((value as &[u8]).into())
    }
}

/// Error type used by `LiteralByteStream`.
#[derive(Debug)]
pub enum LiteralByteStreamError {
    PositionOverflow,
    AdvanceByOutOfBounds,
    PeekOutOfBounds,
    GetSliceOutOfBounds,
}

impl ByteStreamError for LiteralByteStreamError {
    fn position_overflow_error() -> Self {
        Self::PositionOverflow
    }
}

impl ByteStream for LiteralByteStream {
    type Error = LiteralByteStreamError;

    unsafe fn advance_by(&mut self, amount: NonZero<usize>) -> Result<(), Self::Error> {
        let new_position = self.checked_offset(amount.get())?;
        if new_position > self.bytes.len() {
            Err(LiteralByteStreamError::AdvanceByOutOfBounds)
        } else {
            self.position = new_position;
            Ok(())
        }
    }

    unsafe fn peek_offset(&self, offset: usize) -> Result<Option<u8>, Self::Error> {
        let byte_position = self.checked_offset(offset)?;
        match self.bytes.get(byte_position) {
            Some(&byte) => Ok(Some(byte)),
            None if byte_position == self.bytes.len() => Ok(None),
            None => Err(LiteralByteStreamError::PeekOutOfBounds),
        }
    }

    unsafe fn get_slice_offset(
        &self,
        offset: usize,
        length: NonZero<usize>,
    ) -> Result<&[u8], Self::Error> {
        let first_byte_position = self.checked_offset(offset)?;
        if first_byte_position > self.bytes.len() {
            return Err(LiteralByteStreamError::GetSliceOutOfBounds);
        }
        Ok(&self.bytes[first_byte_position
            ..first_byte_position
                .saturating_add(length.get())
                .min(self.bytes.len())])
    }

    fn get_position(&self) -> Result<Position, Self::Error> {
        Ok(Position::new(self.position))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::sample_slice;
    use rand::{
        Rng,
        distr::{Distribution, Uniform},
    };
    use std::ops::Range;

    #[test]
    fn overflows() {
        let mut stream = LiteralByteStream::new(Box::new(*b"content to allow overflow"));
        stream
            .peek()
            .unwrap()
            .expect("`stream` should not be empty");
        // Safe because `stream.peek()` is `Some` at this point
        unsafe {
            stream.advance_by(NonZero::new_unchecked(1)).unwrap();
        }
        // Safe because of `LiteralByteStream` relaxed invariants
        unsafe {
            stream
                .advance_by(NonZero::<usize>::MAX)
                .expect_err("`LiteralByteStream` should prevent overflow in `advance_by`");
            stream
                .peek_offset(usize::MAX)
                .expect_err("`LiteralByteStream` should prevent overflow in `peek_offset`");
            stream
                .get_slice_offset(usize::MAX, NonZero::<usize>::MAX)
                .expect_err("`LiteralByteStream` should prevent overflow in `get_slice_offset`");
        }
    }

    #[test]
    fn random_marchal() {
        const ITERATIONS: usize = 10;
        const CONTENT_LENGTH_RANGE: Range<usize> = 10000..20000;

        let mut rng = rand::rng();
        for _ in 0..ITERATIONS {
            let content_length = rng.random_range(CONTENT_LENGTH_RANGE);
            let content = sample_slice(&mut rng, rand::distr::Alphanumeric, content_length);
            let mut stream = LiteralByteStream::new(content.clone());
            for index in 0..content_length {
                assert_eq!(stream.peek().unwrap(), Some(content[index]));
                // Safe because `stream.peek()` is `Some`
                unsafe { stream.advance_by(NonZero::new_unchecked(1)).unwrap() }
            }
            assert_eq!(stream.peek().unwrap(), None);
        }
    }

    #[test]
    fn random_peeks() {
        const ITERATIONS: usize = 10;
        const CONTENT_LENGTH_RANGE: Range<usize> = 10000..20000;
        const PEEKS_AMOUNT_RANGE: Range<usize> = 20000..40000;

        let mut rng = rand::rng();
        for _ in 0..ITERATIONS {
            let content_length = rng.random_range(CONTENT_LENGTH_RANGE);
            let content = sample_slice(&mut rng, rand::distr::Alphanumeric, content_length);
            let stream = LiteralByteStream::new(content.clone());
            let peeks_amount = rng.random_range(PEEKS_AMOUNT_RANGE);
            let between = Uniform::try_from(0..=content_length).unwrap();
            let mut sample = between.sample_iter(&mut rng);
            for _ in 0..peeks_amount {
                let offset = sample.next().unwrap();
                assert_eq!(
                    // Safe because `offset` is less than or equal to `content_length`
                    unsafe { stream.peek_offset(offset).unwrap() },
                    content.get(offset).copied()
                );
            }
        }
    }

    #[test]
    fn random_slices_within() {
        const CONTENT_LENGTH: usize = 10000;
        const ITERATIONS: usize = 20000;

        let mut rng = rand::rng();
        let content = sample_slice(&mut rng, rand::distr::Alphanumeric, CONTENT_LENGTH);
        let stream = LiteralByteStream::new(content.clone());
        let between = rand::distr::Uniform::try_from(0..=CONTENT_LENGTH).unwrap();
        let mut sample = between.sample_iter(&mut rng);
        for _ in 0..ITERATIONS {
            let mut indices = [0; 2];
            for index in indices.iter_mut() {
                *index = sample.next().unwrap();
            }
            indices.sort();
            let (offset, length) = {
                (
                    indices[0].min(CONTENT_LENGTH - 1),
                    NonZero::new(indices[1] - indices[0])
                        .unwrap_or(unsafe { NonZero::new_unchecked(1) }),
                )
            };
            // Safe because `offset .. offset + length` is a valid slice of `content`
            let slice = unsafe { stream.get_slice_offset(offset, length).unwrap() };
            assert_eq!(*slice, content[offset..offset + length.get()]);
        }
    }

    #[test]
    fn random_slices_past_the_end() {
        const CONTENT_LENGTH: usize = 1000;
        const ITERATIONS: usize = 1000;

        let mut rng = rand::rng();
        let content = sample_slice(&mut rng, rand::distr::Alphanumeric, CONTENT_LENGTH);
        let stream = LiteralByteStream::new(content.clone());
        let between = rand::distr::Uniform::try_from(0..=CONTENT_LENGTH).unwrap();
        let mut sample = between.sample_iter(&mut rng);
        for _ in 0..ITERATIONS {
            let offset = sample.next().unwrap();
            // Safe because `offset` is less than or equal to `CONTENT_LENGTH`
            let slice = unsafe {
                stream
                    .get_slice_offset(offset, NonZero::new_unchecked(CONTENT_LENGTH))
                    .unwrap()
            };
            assert_eq!(*slice, content[offset..]);
        }
    }
}
