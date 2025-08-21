//! This module contains `ByteStream` and `ByteStreamError` traits and `Position` struct, which are
//! core components of byte stream system.

use std::num::NonZero;

/// Return type of `ByteStream::get_position`.
///
/// Positions are used to locate bytes within streams and files. Parsing primitives should store
/// raw positions, which later can be processed by various consumers to extract useful information.
///
/// # Valid positions
/// This type doesn't consider any positions to be invalid. Consumers of `Position`s are free to
/// choose when they are considered valid or not, but any position that is obtained by calling
/// `ByteStream::get_position` and then, possibly, modifying it in a way that it points to some byte
/// within a stream or to the end of stream byte must be considered valid.
#[derive(Clone, Debug, PartialEq)]
pub struct Position(usize);

impl Position {
    pub fn new(position: usize) -> Self {
        Self(position)
    }

    /// Offsets current position by specified signed offset. Returns `None` if overflow occured.
    pub fn offset(self, offset: isize) -> Option<Self> {
        Some(Self(self.0.checked_add_signed(offset)?))
    }

    /// Offsets current position by specified signed offset assuming no overflow would occur.
    ///
    /// # Safety
    /// This method is safe if no overflow will occur after adding `offset` to current position.
    pub unsafe fn offset_unchecked(self, offset: isize) -> Self {
        self.offset(offset).unwrap()
    }
}

/// Required trait for `ByteStream::Error`. Provides constructor methods for required error types.
pub trait ByteStreamError {
    /// Produces error, which signals overflow of stream position.
    fn position_overflow_error() -> Self;
}

/// Wrapper around `usize::checked_add` which returns `S::Error::position_overflow_error` on
/// overflow.
pub fn checked_offset<S>(position: usize, offset: usize) -> Result<usize, S::Error>
where
    S: ByteStream + ?Sized,
{
    match position.checked_add(offset) {
        Some(position) => Ok(position),
        None => Err(S::Error::position_overflow_error()),
    }
}

/// Base interface of interaction with byte streams. Abstracts away tools for inspecting and
/// advancing streams, locating and extracting stream data.
pub trait ByteStream {
    /// Type with all possible errors, that may occur in any of the stream operations. Parsing
    /// functions should propagate these errors if they occur.
    type Error: ByteStreamError;

    /// Consume `amount` bytes from the stream, changing the pointed-to byte.
    ///
    /// # Safety
    /// `self.peek_offset(amount - 1)` must be `Some`. In particular, this function causes UB if
    /// `self.peek()` is `None`, no matter the `amount` (stream can't be advanced past the end).
    unsafe fn advance_by(&mut self, amount: NonZero<usize>) -> Result<(), Self::Error>;

    /// Look up the `offset`th after the pointed-to byte without advancing the stream.
    /// Returns `None` if the `offset`th byte is exactly after the last byte of a stream.
    ///
    /// # Safety
    /// Stream must have greater then or exactly `offset` bytes left (including the pointed-to
    /// byte). Note that this implies, that `self.peek_offset(0)` is always safe.
    unsafe fn peek_offset(&self, offset: usize) -> Result<Option<u8>, Self::Error>;

    /// Equivalent to `self.peek_offset(0)`. See the documentation there.
    fn peek(&self) -> Result<Option<u8>, Self::Error> {
        // Safe because `self.peek_offset(0)` is always safe
        unsafe { self.peek_offset(0) }
    }

    /// Try to get contiguous slice of `length` bytes with given `offset` from the pointed-to byte
    /// without advancing the stream. Returns the slice of maximal length that is less than or equal
    /// to `length`. Returned slice is empty if and only if the stream is exhausted.
    ///
    /// # Safety
    /// This function is safe whenever `self.peek_offset(offset)` is safe. For proper definition of
    /// safety see the documentation for `peek_offset`. Note that this implies that
    /// `self.get_slice_offset(0, length)` is always safe.
    unsafe fn get_slice_offset(
        &self,
        offset: usize,
        length: NonZero<usize>,
    ) -> Result<&[u8], Self::Error>;

    /// Equivalent to `self.get_slice_offset(0, length)`. See the documentation there.
    fn get_slice(&self, length: NonZero<usize>) -> Result<&[u8], Self::Error> {
        // Safe because `self.peek_offset(0)` is always safe
        unsafe { self.get_slice_offset(0, length) }
    }

    /// Obtains the position of the pointed-to byte.
    ///
    /// For more information about positions visit `Position` documentation.
    fn get_position(&self) -> Result<Position, Self::Error>;
}
