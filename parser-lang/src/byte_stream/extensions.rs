//! This module contains extension traits to simplify language specific low-level parsing with byte
//! streams.

use parser_core::byte_stream::{
    core::ByteStream,
    extensions::{StartsWithAnd, TakeWhile},
};
use std::num::NonZero;

fn is_identifier_char(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_'
}

/// Trait that provides `starts_with_ident` family of methods.
pub trait StartsWithIdent: ByteStream {
    /// Runs `self.starts_with_offset_and(identifier, _, offset)` with predicate which ensures that
    /// `identifier` cannot be expanded further with characters that match identifier pattern. This
    /// function doesn't modify the stream.
    ///
    /// # Safety
    /// This function is safe whenever `self.peek_offset(offset)` is safe. Note that this implies
    /// that `self.starts_with_ident_offset(identifier, 0)` is always safe.
    unsafe fn starts_with_ident_offset(
        &self,
        identifier: &[u8],
        offset: usize,
    ) -> Result<bool, Self::Error> {
        // Safe because of function precondition
        unsafe {
            self.starts_with_and_offset(
                identifier,
                |byte| byte.is_none_or(|byte| !is_identifier_char(byte)),
                offset,
            )
        }
    }

    /// Same as `self.starts_with_ident_offset(identifier, 0)`. Stream can be safely advanced by
    /// `identifier.len()` bytes if this returns `true`, but function itself doesn't modify the
    /// stream.
    fn starts_with_ident(&self, identifier: &[u8]) -> Result<bool, Self::Error> {
        // Safe because `self.peek_offset(0)` is always safe
        unsafe { self.starts_with_ident_offset(identifier, 0) }
    }

    /// Runs `self.starts_with_ident(identifier)` and advances stream by `identifier.len()` bytes if
    /// the returned value is `true`.
    fn starts_with_ident_mut(&mut self, identifier: &[u8]) -> Result<bool, Self::Error> {
        if self.starts_with_ident(identifier)? {
            if let Some(length) = NonZero::new(identifier.len()) {
                // Safe because of `starts_with_ident` postcondition
                unsafe { self.advance_by(length)? }
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

impl<S> StartsWithIdent for S where S: ByteStream + ?Sized {}

/// Trait that provides `take_whitespaces` family of methods.
pub trait TakeWhitespaces: ByteStream {
    /// Runs `self.take_while(_)` with predicate `u8::is_ascii_whitespace`. Stream can be safely
    /// advanced by the returned amount of bytes, but function itself doesn't modify the stream.
    fn take_whitespaces(&self) -> Result<usize, Self::Error> {
        self.take_while(|byte| byte.is_ascii_whitespace())
    }

    /// Runs `self.take_whitespaces()` and then advances the stream by the returned amount of bytes.
    fn take_whitespaces_mut(&mut self) -> Result<usize, Self::Error> {
        if let Some(length) = NonZero::new(self.take_whitespaces()?) {
            // Safe because of `take_while` postcondition
            unsafe { self.advance_by(length)? }
            Ok(length.get())
        } else {
            Ok(0)
        }
    }
}

impl<S> TakeWhitespaces for S where S: ByteStream + ?Sized {}
