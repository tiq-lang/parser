//! This module contains the `CodeStreamExtensions` trait, which provides utilities to simplify
//! low-level parsing with code streams.

use super::core::CodeStream;
use std::{mem::MaybeUninit, num::NonZero};

/// Trait that provides `advancing_read` method.
pub trait AdvancingRead: CodeStream {
    /// Extract `destination.len()` bytes from stream to `destination` while advancing it.
    ///
    /// ## Safety
    /// This function is safe whenever `self.peek_offset(destination.len())` is safe. In other
    /// words, stream must have at least `destination.len()` bytes left.
    unsafe fn advancing_read(
        &mut self,
        destination: &mut [MaybeUninit<u8>],
    ) -> Result<(), Self::Error> {
        let mut remaining = destination.len();
        while remaining >= 1 {
            // Safe because of loop condition
            let slice = self.get_slice(unsafe { NonZero::new_unchecked(remaining) })?;
            for (index, &byte) in slice.iter().enumerate() {
                destination[destination.len() - remaining + index].write(byte);
            }
            remaining -= slice.len();
            // Safe because stream has at least `remaining` bytes left (which is > 0)
            // and `get_slice` postcondition
            unsafe { self.advance_by(NonZero::new_unchecked(slice.len()))? };
        }
        Ok(())
    }
}

impl<S: CodeStream + ?Sized> AdvancingRead for S {}

/// Trait that provides `take_while` family of functions.
pub trait TakeWhile<P>: CodeStream
where
    P: FnMut(u8) -> bool,
{
    /// Peeks bytes from stream with provided `offset` and check them against given predicate.
    /// Finishes after the first falsy byte or end of stream and returns the amount of matched
    /// truthy bytes. This function doesn't modify the stream.
    ///
    /// ## Safety
    /// This function is safe whenever `self.peek_offset(offset)` is safe. Note that this implies
    /// that `self.take_while_offset(predicate, 0)` is always safe.
    unsafe fn take_while_offset(
        &self,
        mut predicate: P,
        offset: usize,
    ) -> Result<usize, Self::Error> {
        for matched in 0.. {
            // Safe because of function precondition and loop logic
            if unsafe {
                !self
                    .peek_offset(offset + matched)?
                    .is_some_and(&mut predicate)
            } {
                return Ok(matched);
            }
        }
        unreachable!();
    }

    /// Same as `self.take_while_offset(predicate, 0)`. Stream can be safely advanced by the
    /// returned amount of bytes, but function itself doesn't modify the stream.
    fn take_while(&self, predicate: P) -> Result<usize, Self::Error> {
        // Safe because `self.peek_offset(0)` is always safe
        unsafe { self.take_while_offset(predicate, 0) }
    }

    /// Runs `self.take_while(predicate)` and then advances the stream by the returned amount of
    /// bytes.
    fn take_while_mut(&mut self, predicate: P) -> Result<usize, Self::Error> {
        match self.take_while(predicate)? {
            length @ 1.. => {
                // Safe because of `take_while` postcondition and preceding check
                unsafe { self.advance_by(NonZero::new_unchecked(length))? }
                Ok(length)
            }
            _ => Ok(0),
        }
    }
}

impl<S, P> TakeWhile<P> for S
where
    S: CodeStream + ?Sized,
    P: FnMut(u8) -> bool,
{
}

/// Trait that provides `take_common_prefix` family of functions.
pub trait TakeCommonPrefix: CodeStream {
    /// Peeks bytes from the stream with provided `offset` and checks them for equality with
    /// `prefix`. Finishes after the first mismatched byte or after the prefix is exhausted.
    /// Returns the amount of bytes that match ones from prefix. This function doesn't modify the
    /// stream.
    ///
    /// ## Safety
    /// This function is safe whenever `self.peek_offset(offset)` is safe. Note that this implies
    /// that `self.take_common_prefix_offset(prefix, 0)` is always safe.
    unsafe fn take_common_prefix_offset(
        &self,
        prefix: &[u8],
        offset: usize,
    ) -> Result<usize, Self::Error> {
        for matched in 0..prefix.len() {
            // Safe because of function precondition and loop logic
            if unsafe {
                !self
                    .peek_offset(offset + matched)?
                    .is_some_and(|byte| byte == prefix[matched])
            } {
                return Ok(matched);
            }
        }
        Ok(prefix.len())
    }

    /// Same as `self.take_common_prefix_offset(prefix, 0)`. Stream can be safely advanced by the
    /// returned amount of bytes, but function itself doesn't modify the stream.
    fn take_common_prefix(&self, prefix: &[u8]) -> Result<usize, Self::Error> {
        unsafe { self.take_common_prefix_offset(prefix, 0) }
    }
}

impl<S: CodeStream + ?Sized> TakeCommonPrefix for S {}

/// Trait that provides `starts_with` family of functions.
pub trait StartsWith: CodeStream {
    /// Runs `self.take_common_prefix_offset(bytes, offset)` and checks if returned amount of bytes
    /// matches `bytes.len()`.
    ///
    /// ## Safety
    /// This function is safe whenever `self.peek_offset(offset)` is safe. Note that this implies
    /// that `self.starts_with_offset(predicate, 0)` is always safe.
    unsafe fn starts_with_offset(&self, bytes: &[u8], offset: usize) -> Result<bool, Self::Error> {
        // Safe because of function precondition
        unsafe { Ok(self.take_common_prefix_offset(bytes, offset)? == bytes.len()) }
    }

    /// Same as `self.starts_with_offset(bytes, 0)`. Stream can be safely advanced by `bytes.len()`
    /// bytes if this returns `true`, but function itself doesn't modify the stream.
    fn starts_with(&self, bytes: &[u8]) -> Result<bool, Self::Error> {
        // Safe because `self.peek_offset(0)` is always safe
        unsafe { self.starts_with_offset(bytes, 0) }
    }

    /// Runs `self.starts_with(bytes)` and then advances the stream by `bytes.len()` bytes if the
    /// returned value is `true`.
    fn starts_with_mut(&mut self, bytes: &[u8]) -> Result<bool, Self::Error> {
        if self.starts_with(bytes)? {
            if let length @ 1.. = bytes.len() {
                // Safe because of `starts_with` postcondition and if-condition
                unsafe { self.advance_by(NonZero::new_unchecked(length))? }
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

impl<S: CodeStream + ?Sized> StartsWith for S {}

/// Trait that provides `starts_with_and` family of functions.
pub trait StartsWithAnd<P>: CodeStream
where
    P: FnOnce(Option<u8>) -> bool,
{
    /// Runs `self.starts_with_offset(bytes, offset)` and if it returns `true` runs given predicate
    /// with `self.peek_offset(offset + bytes.len())` as an argument.
    ///
    /// ## Safety
    /// This function is safe whenever `self.peek_offset(offset)` is safe. Note that this implies
    /// that `self.starts_with_and_offset(bytes, predicate, 0)` is always safe.
    unsafe fn starts_with_and_offset(
        &self,
        bytes: &[u8],
        predicate: P,
        offset: usize,
    ) -> Result<bool, Self::Error> {
        // Safe because of function precondition and `starts_with_offset` postcondition
        if unsafe {
            self.starts_with_offset(bytes, offset)?
                && predicate(self.peek_offset(offset + bytes.len())?)
        } {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Same as `self.starts_with_and_offset(bytes, predicate, 0)`. Stream can be safely advanced by
    /// `bytes.len()` bytes if this returns `true`, but function itself doesn't modify the stream.
    fn starts_with_and(&self, bytes: &[u8], predicate: P) -> Result<bool, Self::Error> {
        // Safe because `self.peek_offset(0)` is always safe
        unsafe { self.starts_with_and_offset(bytes, predicate, 0) }
    }

    /// Runs `self.starts_with_and(bytes, predicate)` and then advances the stream by `bytes.len()`
    /// bytes if the returned value is `true`.
    fn starts_with_and_mut(&mut self, bytes: &[u8], predicate: P) -> Result<bool, Self::Error> {
        if self.starts_with_and(bytes, predicate)? {
            if let length @ 1.. = bytes.len() {
                // Safe because of `starts_with_and` postcondition and if-condition
                unsafe { self.advance_by(NonZero::new_unchecked(length))? }
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

impl<S, P> StartsWithAnd<P> for S
where
    S: CodeStream + ?Sized,
    P: FnOnce(Option<u8>) -> bool,
{
}
