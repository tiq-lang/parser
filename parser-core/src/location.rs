//! This module contains data structures, that can be used to locate parsing primitives.

use crate::{
    code_stream::core::{CodeSpan, GetSpanInfoResult, SpanOrigin},
    origin,
};

/// Generic structure to point to some type `T` within a file or to the synthetic orgin.
#[derive(Clone, Copy, Debug)]
pub enum Located<T> {
    File(&'static origin::File, T),
    Macro(&'static origin::Macro),
    Derive(&'static origin::Derive),
    Attribute(&'static origin::Attribute),
}

impl<T> Located<T> {
    /// Constructs new `Located` from `SpanOrigin`. If `span_origin` is `SpanOrigin::File` invokes
    /// provided `mapping` using matched `position_in_file` to produce a value of `T`, otherwise
    /// simply propagates matched `origin`.
    pub fn from_span_origin<F>(span_origin: &SpanOrigin, mapping: F) -> Self
    where
        F: FnOnce(usize) -> T,
    {
        match *span_origin {
            SpanOrigin::File {
                file,
                position_in_file,
            } => Self::File(file, mapping(position_in_file)),
            SpanOrigin::Macro { r#macro } => Self::Macro(r#macro),
            SpanOrigin::Derive { derive } => Self::Derive(derive),
            SpanOrigin::Attribute { attribute } => Self::Attribute(attribute),
        }
    }
}

/// Newtype around `Located<usize>`. Points to the position in file or to the synthetic origin.
pub struct Location(Located<usize>);

impl<'a, S: CodeSpan + ?Sized> From<GetSpanInfoResult<'a, S>> for Location {
    fn from(value: GetSpanInfoResult<'a, S>) -> Self {
        Self(Located::from_span_origin(
            value.span.get_origin(),
            |position_in_file| position_in_file + value.offset_in_span,
        ))
    }
}
