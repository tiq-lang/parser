//! This module contains `CodeStreamView` struct, which provides a way to create a 'detached' view
//! over the stream, that is easier to work with than with raw offsets.

use super::core::{CodeStream, GetSpanInfoResult};
use std::num::NonZero;

/// Code stream over another stream, that has some offset from it.
#[derive(Clone, Copy)]
pub struct CodeStreamView<'a, S: CodeStream + ?Sized + 'a> {
    stream: &'a S,
    offset: usize,
}

impl<'a, S: CodeStream + ?Sized + 'a> CodeStreamView<'a, S> {
    pub fn new(stream: &'a S) -> Self {
        Self { stream, offset: 0 }
    }

    pub fn get_offset(&self) -> usize {
        self.offset
    }

    pub unsafe fn get_span_info_offset(
        &self,
        offset: usize,
    ) -> Result<GetSpanInfoResult<'a, S::Span>, S::Error> {
        // Safe if `advance_by` invariant is upheld
        unsafe { self.stream.get_span_info_offset(self.offset + offset) }
    }

    pub fn get_span_info(&self) -> Result<GetSpanInfoResult<'a, S::Span>, S::Error> {
        // Safe because `self.peek_nth(0)` is always safe
        unsafe { self.get_span_info_offset(0) }
    }
}

impl<'a, S: CodeStream + ?Sized + 'a> CodeStream for CodeStreamView<'a, S> {
    type Error = S::Error;
    type Span = S::Span;

    unsafe fn advance_by(&mut self, amount: NonZero<usize>) -> Result<(), Self::Error> {
        self.offset += amount.get();
        Ok(())
    }

    unsafe fn peek_offset(&self, offset: usize) -> Result<Option<u8>, Self::Error> {
        // Safe if `advance_by` invariant is upheld
        unsafe { self.stream.peek_offset(self.offset + offset) }
    }

    unsafe fn get_slice_offset(
        &self,
        offset: usize,
        length: NonZero<usize>,
    ) -> Result<&[u8], Self::Error> {
        // Safe because of function precondition
        unsafe { self.stream.get_slice_offset(self.offset + offset, length) }
    }

    unsafe fn get_span_info_offset(
        &self,
        offset: usize,
    ) -> Result<GetSpanInfoResult<Self::Span>, Self::Error> {
        // Safe because of function precondition
        unsafe { self.get_span_info_offset(self.offset + offset) }
    }
}
