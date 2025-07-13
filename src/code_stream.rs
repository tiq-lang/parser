//! TODO: add docs

use std::{
    mem::MaybeUninit,
    num::NonZero,
    ops::{ControlFlow, Range},
    path::Path,
    rc::Rc,
};

/// TODO: write docs
#[derive(Clone, Copy, Debug)]
pub enum SyntheticSpanSource {
    Macro,
    Derive,
    Attribute,
}

/// TODO: write docs
#[derive(Clone, Debug)]
pub enum SpanKind {
    Natural {
        file: Rc<Path>,
        position_in_file: usize,
    },
    Synthetic {
        source: SyntheticSpanSource,
        file: Rc<Path>,
        range: Range<usize>,
    },
}

/// TODO: write docs
pub struct SpanInfo {
    pub kind: SpanKind,
    pub position_in_stream: usize,
}

/// TODO: write docs
#[derive(Clone, Copy)]
pub struct GetSpanInfoResult<'a> {
    pub span_info: &'a SpanInfo,
    pub offset_in_span: usize,
}

pub enum StreamSource {
    File(Rc<Path>),
    Macro(Rc<Path>, Range<usize>),
    Derive(Rc<Path>, Range<usize>),
    Attribute(Rc<Path>, Range<usize>),
}

pub trait CodeStream {
    /// Type with all possible errors, that may occur in any of the stream operations.
    /// Parsing functions simply propagate these errors and do not handle them.
    type Error;

    /// Equivalent to `self.peek_nth(0)`. See the documentation there.
    fn peek(&self) -> Result<Option<u8>, Self::Error> {
        // SAFETY: this is safe if `advance_by` invariant is upheld
        unsafe { self.peek_nth(0) }
    }

    /// Look up the `offset`th after the pointed-to byte without advancing the stream.
    /// Returns `None` if the `offset`th byte is exactly after the last byte of a stream.
    ///
    /// ## Safety
    /// Stream must have greater then or exactly `offset` bytes left (including the
    /// pointed-to byte). Note that this implies, that `self.peek_nth(0)` is always safe.
    unsafe fn peek_nth(&self, offset: usize) -> Result<Option<u8>, Self::Error>;

    /// Consume `amount` bytes from the stream, changing the pointed-to byte.
    ///
    /// ## Safety
    /// `self.peek_nth(amount - 1)` must be `Some`. In particular,
    /// this function causes UB if `self.peek()` is `None`, no matter the `amount`
    /// (stream can't be advanced past the end).
    unsafe fn advance_by(&mut self, amount: NonZero<usize>) -> Result<(), Self::Error>;

    /// Equivalent to `self.get_slice_offset(0, length)`. See the documentation there.
    fn get_slice<'a>(&'a self, length: NonZero<usize>) -> Result<&'a [u8], Self::Error> {
        // SAFETY: this is safe if `advance_by` invariant is upheld
        unsafe { self.get_slice_offset(0, length) }
    }

    /// Try to get contiguous slice of `length` bytes with given `offset` from the
    /// pointed-to byte without advancing the stream. Upon success returns the slice
    /// of desired length, otherwise returns the slice of bytes with maximal possible
    /// length, that is less than `length`. Returned slice is empty only if the stream
    /// is exhausted.
    ///
    /// ## Safety
    /// This function is safe whenever `self.peek_nth(offset)` is safe. For proper
    /// definition of safety see the documentation for `peek_nth`. Note that this implies
    /// that `self.get_slice_offset(0, length)` is always safe.
    unsafe fn get_slice_offset<'a>(
        &'a self,
        offset: usize,
        length: NonZero<usize>,
    ) -> Result<&'a [u8], Self::Error>;

    /// Provides information about the span located after `offset` bytes from currently viewed byte.
    /// If this function is called when `self.peek_nth(offset)` is `None`, returns span information
    /// as if this 'byte' was located inside of the last span.
    ///
    /// ## Safety
    /// This function is safe when `self.peek_nth(offset)` is safe. Note that this implies
    /// that `self.get_span_info_offset(0)` is always safe.
    unsafe fn get_span_info_offset<'a>(
        &'a self,
        offset: usize,
    ) -> Result<GetSpanInfoResult<'a>, Self::Error>;

    /// Same as `self.get_span_info_offset(0)`. See the documentation there.
    fn get_span_info<'a>(&'a self) -> Result<GetSpanInfoResult<'a>, Self::Error> {
        // Safe because `self.peek_nth(0)` is always safe
        unsafe { self.get_span_info_offset(0) }
    }

    /// Provides information about the source of the stream. In other words, it answers the question
    /// of 'Why am I currently performing parsing?'. It can point to macro/derive/attribute
    /// invocation or to a file.
    fn get_stream_source<'a>(&'a self) -> &'a StreamSource;
}

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
    ) -> Result<GetSpanInfoResult<'a>, S::Error> {
        // Safe if `advance_by` invariant is upheld
        unsafe { self.stream.get_span_info_offset(self.offset + offset) }
    }

    pub fn get_span_info(&self) -> Result<GetSpanInfoResult<'a>, S::Error> {
        // Safe because `self.peek_nth(0)` is always safe
        unsafe { self.get_span_info_offset(0) }
    }

    pub fn get_stream_source(&self) -> &'a StreamSource {
        self.stream.get_stream_source()
    }
}

impl<'a, S: CodeStream + ?Sized + 'a> CodeStream for CodeStreamView<'a, S> {
    type Error = S::Error;

    unsafe fn peek_nth(&self, offset: usize) -> Result<Option<u8>, Self::Error> {
        // Safe if `advance_by` invariant is upheld
        unsafe { self.stream.peek_nth(self.offset + offset) }
    }

    unsafe fn advance_by(&mut self, amount: NonZero<usize>) -> Result<(), Self::Error> {
        self.offset += amount.get();
        Ok(())
    }

    unsafe fn get_slice_offset<'b>(
        &'b self,
        offset: usize,
        length: NonZero<usize>,
    ) -> Result<&'b [u8], Self::Error> {
        // Safe because of function precondition
        unsafe { self.stream.get_slice_offset(self.offset + offset, length) }
    }

    unsafe fn get_span_info_offset<'b>(
        &'b self,
        offset: usize,
    ) -> Result<GetSpanInfoResult<'a>, Self::Error> {
        // Safe because of function precondition
        unsafe { self.get_span_info_offset(offset) }
    }

    fn get_stream_source<'b>(&'b self) -> &'b StreamSource {
        self.get_stream_source()
    }
}

pub trait CodeStreamExtensions: CodeStream {
    /// Extract `destination.len()` bytes from stream to `destination` while advancing it.
    ///
    /// ## Safety
    /// This function is safe whenever `self.peek_nth(destination.len())` is safe. In other words,
    /// stream must have at least `destination.len()` bytes left.
    unsafe fn advancing_read(
        &mut self,
        destination: &mut [MaybeUninit<u8>],
    ) -> Result<(), Self::Error> {
        let mut remaining = destination.len();
        while remaining >= 1 {
            // Safe because of while condition
            let slice = unsafe { self.get_slice(NonZero::new_unchecked(remaining))? };
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

    /// Peek `offset`th byte from the stream and check it against given predicate. This function
    /// doesn't modify the stream.
    ///
    /// ## Safety
    /// This function is safe whenever `self.peek_nth(offset)` is safe. Note that this implies that
    /// `self.filter_offset(predicate, 0)` is always safe.
    unsafe fn filter_offset<P>(&self, predicate: P, offset: usize) -> Result<bool, Self::Error>
    where
        P: FnOnce(u8) -> bool,
    {
        // Safe because of function precondition
        match unsafe { self.peek_nth(offset)? } {
            Some(byte) if predicate(byte) => Ok(true),
            _ => Ok(false),
        }
    }

    /// Same as `self.filter_offset(predicate, 0)`. If this returns `true`, stream can be safely
    /// advanced by one byte, but function itself doesn't modify the stream.
    fn filter<P>(&self, predicate: P) -> Result<bool, Self::Error>
    where
        P: FnOnce(u8) -> bool,
    {
        // Safe because `self.peek_nth(0)` is always safe
        unsafe { self.filter_offset(predicate, 0) }
    }

    fn filter_mut<P>(&mut self, predicate: P) -> Result<bool, Self::Error>
    where
        P: FnOnce(u8) -> bool,
    {
        if self.filter(predicate)? {
            // Safe because of `filter` postcondition
            unsafe { self.advance_by(NonZero::new_unchecked(1))? }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Peek `offset`th byte from the stream and apply provided mapping to it. This function doesn't
    /// modify the stream.
    ///
    /// ## Safety
    /// This function is safe whenever `self.peek_nth(offset)` is safe. Note that this implies that
    /// `self.map_offset(mapping, 0)` is always safe.
    unsafe fn map_offset<M, R>(&self, mapping: M, offset: usize) -> Result<R, Self::Error>
    where
        M: FnOnce(Option<u8>) -> R,
    {
        // Safe because of function precondition
        match unsafe { self.peek_nth(offset) } {
            Ok(char) => Ok(mapping(char)),
            Err(error) => Err(error),
        }
    }

    /// Same as `self.map_offset(mapping, 0)`. This function doesn't modify the stream.
    fn map<M, R>(&self, mapping: M) -> Result<R, Self::Error>
    where
        M: FnOnce(Option<u8>) -> R,
    {
        // Safe because `self.peek_nth(0)` is always safe
        unsafe { self.map_offset(mapping, 0) }
    }

    /// Peek substring from stream with provided `offset` and run `function` on every byte until
    /// `function` returns `Break` or stream is exhausted. This function doesn't modify the stream.
    ///
    /// ## Safety
    /// This function is safe whenever `self.peek_nth(offset)` is safe. Note that this implies that
    /// `self.for_each_offset(function, 0)` is always safe.
    unsafe fn for_each_offset<F>(&self, mut function: F, offset: usize) -> Result<(), Self::Error>
    where
        F: FnMut(Option<u8>) -> ControlFlow<()>,
    {
        let mut processed = 0;
        loop {
            // Safe because of function precondition and loop logic
            match unsafe { self.peek_nth(offset + processed)? } {
                Some(byte) => {
                    if function(Some(byte)).is_break() {
                        break;
                    }
                }
                None => {
                    _ = function(None);
                    break;
                }
            }
            processed += 1;
        }
        Ok(())
    }

    /// Same as `self.for_each_offset(function, 0)`. This function doesn't modify the stream.
    fn for_each<F>(&self, function: F) -> Result<(), Self::Error>
    where
        F: FnMut(Option<u8>) -> ControlFlow<()>,
    {
        // Safe because `self.peek_nth(0)` is always safe
        unsafe { self.for_each_offset(function, 0) }
    }

    /// Peek substring from stream with provided `offset` and check its bytes against given
    /// predicate. Finishes after the first falsy byte or end of stream and returns the amount of
    /// matched truthy bytes. This function doesn't modify the stream.
    ///
    /// ## Safety
    /// This function is safe whenever `self.peek_nth(offset)` is safe. Note that this implies that
    /// `self.take_while_offset(predicate, 0)` is always safe.
    unsafe fn take_while_offset<P>(
        &self,
        mut predicate: P,
        offset: usize,
    ) -> Result<usize, Self::Error>
    where
        P: FnMut(u8) -> bool,
    {
        let mut matched = 0;
        loop {
            // Safe because of function precondition and `read_if_offset` postcondition
            if unsafe { !self.filter_offset(&mut predicate, offset + matched)? } {
                return Ok(matched);
            }
            matched += 1;
        }
    }

    /// Same as `self.take_while_offset(predicate, 0)`. Stream can be safely advanced by the
    /// returned amount of bytes, but function itself doesn't modify the stream.
    fn take_while<P>(&self, predicate: P) -> Result<usize, Self::Error>
    where
        P: FnMut(u8) -> bool,
    {
        // Safe because `self.peek_nth(0)` is always safe
        unsafe { self.take_while_offset(predicate, 0) }
    }

    /// TODO: write docs
    fn take_while_mut<P>(&mut self, predicate: P) -> Result<usize, Self::Error>
    where
        P: FnMut(u8) -> bool,
    {
        let steps = self.take_while(predicate)?;
        if steps != 0 {
            // Safe because of `take_while` postcondition and preceding check
            unsafe { self.advance_by(NonZero::new_unchecked(steps))? }
        }
        Ok(steps)
    }

    /// TODO: write docs
    unsafe fn starts_with_offset(&self, bytes: &[u8], offset: usize) -> Result<usize, Self::Error> {
        for matched in 0..bytes.len() {
            // Safe because of function precondition and `read_if_offset` postcondition
            if unsafe { !self.filter_offset(|byte| byte == bytes[matched], offset + matched)? } {
                return Ok(matched);
            }
        }
        Ok(bytes.len())
    }

    /// Same as `self.starts_with_offset(bytes, 0)`. Stream can be safely advanced by the returned
    /// amount of bytes, but function itself doesn't modify the stream.
    fn starts_with(&self, bytes: &[u8]) -> Result<usize, Self::Error> {
        // Safe because `self.peek_nth(0)` is always safe
        unsafe { self.starts_with_offset(bytes, 0) }
    }

    /// TODO: write docs
    fn starts_with_mut(&mut self, bytes: &[u8]) -> Result<usize, Self::Error> {
        let matched = self.starts_with(bytes)?;
        if matched != 0 && matched == bytes.len() {
            // Safe because of `starts_with` postcondition and preceding
            unsafe { self.advance_by(NonZero::new_unchecked(matched))? }
        }
        Ok(matched)
    }
}

impl<S: CodeStream + ?Sized> CodeStreamExtensions for S {}

pub struct LiteralCodeStream {
    bytes: Box<[u8]>,
    spans: Box<[SpanInfo]>,
    position: usize,
    source: StreamSource,
}

impl LiteralCodeStream {
    pub fn new(raw_spans: &[(&[u8], SpanKind)], source: StreamSource) -> Self {
        assert_ne!(raw_spans.len(), 0);
        let length = raw_spans.iter().map(|&(bytes, _)| bytes.len()).sum();
        let mut bytes = Box::<[u8]>::new_uninit_slice(length);
        let mut spans = Box::<[SpanInfo]>::new_uninit_slice(raw_spans.len());
        for (index, &(span_bytes, ref span_kind)) in raw_spans.iter().enumerate() {
            let position_in_stream = if index == 0 {
                0
            } else {
                // Safe because `spans[index - 1]` was initialized on previous iteration
                let previous_span = unsafe { spans[index - 1].assume_init_ref() };
                previous_span.position_in_stream + raw_spans[index - 1].0.len()
            };
            for (index, &byte) in span_bytes.iter().enumerate() {
                bytes[position_in_stream + index].write(byte);
            }
            spans[index].write(SpanInfo {
                kind: (*span_kind).clone(),
                position_in_stream,
            });
        }
        Self {
            // Safe because `bytes` and `spans` were initialized inside of a loop
            bytes: unsafe { bytes.assume_init() },
            spans: unsafe { spans.assume_init() },
            position: 0,
            source,
        }
    }

    pub fn get_position(&self) -> usize {
        self.position
    }
}

impl CodeStream for LiteralCodeStream {
    type Error = &'static str;

    unsafe fn peek_nth(&self, offset: usize) -> Result<Option<u8>, Self::Error> {
        if self.position + offset > self.bytes.len() {
            Err("`peek_nth` out of bounds")
        } else {
            Ok(self.bytes.get(self.position + offset).cloned())
        }
    }

    unsafe fn advance_by(&mut self, amount: NonZero<usize>) -> Result<(), Self::Error> {
        if self.position + amount.get() > self.bytes.len() {
            Err("`advance_by` out of bounds")
        } else {
            self.position += amount.get();
            Ok(())
        }
    }

    unsafe fn get_slice_offset<'a>(
        &'a self,
        offset: usize,
        length: NonZero<usize>,
    ) -> Result<&'a [u8], Self::Error> {
        match self
            .bytes
            .get((self.position + offset)..(self.position + offset + length.get()))
        {
            Some(slice) => Ok(slice),
            None => Err("`get_slice_offset` out of bounds"),
        }
    }

    unsafe fn get_span_info_offset<'a>(
        &'a self,
        offset: usize,
    ) -> Result<GetSpanInfoResult<'a>, Self::Error> {
        if self.position + offset > self.bytes.len() {
            return Err("`get_span_info_offset` out of bounds");
        }
        let span_index = match self
            .spans
            .partition_point(|span_info| span_info.position_in_stream <= self.position + offset)
        {
            0 => unreachable!(),
            x => x - 1,
        };
        let span_info = &self.spans[span_index];
        let offset_in_span = self.position + offset - span_info.position_in_stream;
        Ok(GetSpanInfoResult {
            span_info,
            offset_in_span,
        })
    }

    fn get_stream_source<'a>(&'a self) -> &'a StreamSource {
        &self.source
    }
}
