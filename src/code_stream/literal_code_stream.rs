//! This module contains the `LiteralCodeStream` struct, which provides a way to create a code
//! stream over fixed slice of bytes.

use super::core::{CodeSpan, CodeStream, GetSpanInfoResult, SpanId, SpanOrigin};
use std::num::NonZero;

/// Code span type used by `LiteralCodeStream`.
#[derive(Debug, PartialEq)]
pub struct LiteralCodeSpan {
    origin: SpanOrigin,
    position_in_stream: usize,
}

impl CodeSpan for LiteralCodeSpan {
    fn get_id(&self) -> SpanId {
        SpanId(unsafe { NonZero::new_unchecked(self as *const _ as _) })
    }

    fn get_origin(&self) -> &SpanOrigin {
        &self.origin
    }
}

/// Code stream over fixed slice of bytes. Implementation of `LiteralCodeStream` guarantees that
/// all of the methods of `CodeStream` trait are safe.
pub struct LiteralCodeStream {
    bytes: Box<[u8]>,
    spans: Box<[LiteralCodeSpan]>,
    position: usize,
}

impl LiteralCodeStream {
    pub fn new<T: AsRef<[u8]>>(raw_spans: &[(T, SpanOrigin)]) -> Self {
        assert_ne!(raw_spans.len(), 0, "`raw_spans` should be non-empty");
        let length = raw_spans
            .iter()
            .map(|&(ref bytes, _)| bytes.as_ref().len())
            .sum();
        let mut bytes = Box::<[u8]>::new_uninit_slice(length);
        let mut spans = Box::<[LiteralCodeSpan]>::new_uninit_slice(raw_spans.len());
        for (index, &(ref span_bytes, ref span_origin)) in raw_spans.iter().enumerate() {
            let position_in_stream = if index == 0 {
                0
            } else {
                // Safe because `spans[index - 1]` was initialized on previous iteration
                let previous_span = unsafe { spans[index - 1].assume_init_ref() };
                previous_span.position_in_stream + raw_spans[index - 1].0.as_ref().len()
            };
            for (index, &byte) in span_bytes.as_ref().iter().enumerate() {
                bytes[position_in_stream + index].write(byte);
            }
            spans[index].write(LiteralCodeSpan {
                origin: (*span_origin).clone(),
                position_in_stream,
            });
        }
        Self {
            // Safe because `bytes` and `spans` were initialized inside of a loop
            bytes: unsafe { bytes.assume_init() },
            spans: unsafe { spans.assume_init() },
            position: 0,
        }
    }

    pub fn get_position(&self) -> usize {
        self.position
    }

    fn offset_position(&self, offset: usize) -> Result<usize, LiteralCodeStreamError> {
        match self.position.checked_add(offset) {
            Some(position) => Ok(position),
            None => Err(LiteralCodeStreamError::PositionOverflow),
        }
    }
}

/// Error type used by `LiteralCodeStream`.
#[derive(Debug)]
pub enum LiteralCodeStreamError {
    PositionOverflow,
    AdvanceByOutOfBounds,
    PeekOutOfBounds,
    GetSliceOutOfBounds,
    GetSpanInfoOutOfBounds,
}

impl CodeStream for LiteralCodeStream {
    type Error = LiteralCodeStreamError;
    type Span = LiteralCodeSpan;

    unsafe fn advance_by(&mut self, amount: NonZero<usize>) -> Result<(), Self::Error> {
        let new_position = self.offset_position(amount.get())?;
        if new_position > self.bytes.len() {
            Err(LiteralCodeStreamError::AdvanceByOutOfBounds)
        } else {
            self.position = new_position;
            Ok(())
        }
    }

    unsafe fn peek_offset(&self, offset: usize) -> Result<Option<u8>, Self::Error> {
        let byte_position = self.offset_position(offset)?;
        match self.bytes.get(byte_position) {
            Some(&byte) => Ok(Some(byte)),
            None if byte_position == self.bytes.len() => Ok(None),
            None => Err(LiteralCodeStreamError::PeekOutOfBounds),
        }
    }

    unsafe fn get_slice_offset(
        &self,
        offset: usize,
        length: NonZero<usize>,
    ) -> Result<&[u8], Self::Error> {
        let first_byte_position = self.offset_position(offset)?;
        if first_byte_position > self.bytes.len() {
            return Err(LiteralCodeStreamError::GetSliceOutOfBounds);
        }
        Ok(&self.bytes[first_byte_position
            ..first_byte_position
                .saturating_add(length.get())
                .min(self.bytes.len())])
    }

    unsafe fn get_span_info_offset(
        &self,
        offset: usize,
    ) -> Result<GetSpanInfoResult<Self::Span>, Self::Error> {
        let byte_position = self.offset_position(offset)?;
        if byte_position > self.bytes.len() {
            return Err(LiteralCodeStreamError::GetSpanInfoOutOfBounds);
        }
        let span_index = match self
            .spans
            .partition_point(|span_info| span_info.position_in_stream <= byte_position)
        {
            0 => unreachable!(),
            x => x - 1,
        };
        let span = &self.spans[span_index];
        let offset_in_span = byte_position - span.position_in_stream;
        Ok(GetSpanInfoResult {
            span,
            offset_in_span,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        origin::{self, Leak},
        testing::{BuildRandomStreamResult, RandomStreamBuilder, sample_slice},
    };
    use rand::{
        Rng,
        distr::{Distribution, Uniform},
    };
    use std::{ops::Range, path::Path, rc::Rc, usize};

    #[test]
    fn overflows() {
        let file: Rc<Path> = Path::new("/file.tiq").into();
        let origin = origin::File(file.clone()).leak();
        let mut stream = LiteralCodeStream::new(&[(
            "content to allow overflow",
            SpanOrigin::File {
                file: origin,
                position_in_file: 0,
            },
        )]);
        stream
            .peek()
            .unwrap()
            .expect("`stream` should not be empty");
        // Safe because `stream.peek()` is `Some` at this point
        unsafe {
            stream.advance_by(NonZero::new_unchecked(1)).unwrap();
        }
        // Safe because of `LiteralCodeStream` relaxed invariants
        unsafe {
            stream
                .advance_by(NonZero::<usize>::MAX)
                .expect_err("`LiteralCodeStream` should prevent overflow in `advance_by`");
            stream
                .peek_offset(usize::MAX)
                .expect_err("`LiteralCodeStream` should prevent overflow in `peek_offset`");
            stream
                .get_slice_offset(usize::MAX, NonZero::<usize>::MAX)
                .expect_err("`LiteralCodeStream` should prevent overflow in `get_slice_offset`");
            stream.get_span_info_offset(usize::MAX).expect_err(
                "`LiteralCodeStream` should prevent overflow in `get_span_info_offset`",
            );
        }
    }

    #[test]
    fn empty_span_info() {
        let file: Rc<Path> = Path::new("/file.tiq").into();
        let origin = origin::File(file.clone()).leak();
        let stream = LiteralCodeStream::new(&[(
            "",
            SpanOrigin::File {
                file: origin,
                position_in_file: 0,
            },
        )]);
        let span_info = stream.get_span_info().unwrap();
        assert_eq!(span_info.offset_in_span, 0);
        assert_eq!(
            *span_info.span,
            LiteralCodeSpan {
                origin: SpanOrigin::File {
                    file: origin,
                    position_in_file: 0
                },
                position_in_stream: 0
            }
        );
    }

    #[test]
    fn random_marchal() {
        const ITERATIONS: usize = 100;
        const CONTENT_LENGTH_RANGE: Range<usize> = 50..250;

        let file: Rc<Path> = Path::new("/file.tiq").into();
        let origin = origin::File(file.clone()).leak();
        let mut rng = rand::rng();
        for _ in 0..ITERATIONS {
            let content_length = rng.random_range(CONTENT_LENGTH_RANGE);
            let content = sample_slice(&mut rng, rand::distr::Alphanumeric, content_length);
            let mut stream = LiteralCodeStream::new(&[(
                &*content,
                SpanOrigin::File {
                    file: origin,
                    position_in_file: 0,
                },
            )]);
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
        const CONTENT_LENGTH_RANGE: Range<usize> = 50..250;
        const PEEKS_AMOUNT_RANGE: Range<usize> = 100..500;

        let file: Rc<Path> = Path::new("/file.tiq").into();
        let origin = origin::File(file.clone()).leak();
        let mut rng = rand::rng();
        for _ in 0..ITERATIONS {
            let content_length = rng.random_range(CONTENT_LENGTH_RANGE);
            let content = sample_slice(&mut rng, rand::distr::Alphanumeric, content_length);
            let stream = LiteralCodeStream::new(&[(
                &*content,
                SpanOrigin::File {
                    file: origin,
                    position_in_file: 0,
                },
            )]);
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
        const CONTENT_LENGTH: usize = 1000;
        const ITERATIONS: usize = 100;

        let file: Rc<Path> = Path::new("/file.tiq").into();
        let mut rng = rand::rng();
        let content = sample_slice(&mut rng, rand::distr::Alphanumeric, CONTENT_LENGTH);
        let BuildRandomStreamResult { stream, .. } = RandomStreamBuilder::new()
            .append_content_from_slice(&*content)
            .append_span_origin(&SpanOrigin::File {
                file: origin::File(file.clone()).leak(),
                position_in_file: 0,
            })
            .build(&mut rng);
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
        const CONTENT_LENGTH: usize = 100;
        const ITERATIONS: usize = 100;

        let file: Rc<Path> = Path::new("/file.tiq").into();
        let mut rng = rand::rng();
        let content = sample_slice(&mut rng, rand::distr::Alphanumeric, CONTENT_LENGTH);
        let BuildRandomStreamResult { stream, .. } = RandomStreamBuilder::new()
            .append_content_from_slice(&*content)
            .append_span_origin(&SpanOrigin::File {
                file: origin::File(file.clone()).leak(),
                position_in_file: 0,
            })
            .build(&mut rng);
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

    #[test]
    fn random_span_infos() {
        const CONTENT_LENGTH: usize = 1000;
        const ITERATIONS: usize = 100;

        let file: Rc<Path> = Path::new("/file.tiq").into();
        let span_origins = [
            SpanOrigin::File {
                file: origin::File(file.clone()).leak(),
                position_in_file: 0,
            },
            SpanOrigin::Macro {
                r#macro: origin::Macro(file.clone(), 0..10).leak(),
            },
            SpanOrigin::Derive {
                derive: origin::Derive(file.clone(), 0..10).leak(),
            },
            SpanOrigin::Attribute {
                attribute: origin::Attribute(file.clone(), 0..10).leak(),
            },
        ];
        let mut rng = rand::rng();
        let BuildRandomStreamResult { stream, splits } = RandomStreamBuilder::new()
            .append_content_from_iter(
                (&mut rng)
                    .sample_iter(rand::distr::Alphanumeric)
                    .take(CONTENT_LENGTH),
            )
            .append_span_origins_from_slice(&span_origins)
            .build(&mut rng);
        let between = rand::distr::Uniform::try_from(0..=CONTENT_LENGTH).unwrap();
        let mut sample = between.sample_iter(&mut rng);
        for _ in 0..ITERATIONS {
            let offset = sample.next().unwrap();
            let span_index = splits.partition_point(|&span_offset| span_offset <= offset) - 1;
            // Safe because `offset` is less than or equal to `CONTENT_LENGTH`
            let span_info = unsafe { stream.get_span_info_offset(offset).unwrap() };
            assert_eq!(span_info.offset_in_span, offset - splits[span_index]);
            assert_eq!(
                *span_info.span,
                LiteralCodeSpan {
                    origin: span_origins[span_index],
                    position_in_stream: splits[span_index],
                }
            );
        }
    }
}
