//! This module contains some utilifies that might be useful for testing.

use crate::code_stream::{core::SpanOrigin, literal_code_stream::LiteralCodeStream};
use rand::{Rng, distr::Distribution, rngs::ThreadRng};

#[derive(Default)]
pub struct RandomStreamBuilder {
    content: Vec<u8>,
    span_origins: Vec<SpanOrigin>,
}

pub struct BuildRandomStreamResult {
    pub stream: LiteralCodeStream,
    pub splits: Box<[usize]>,
}

impl RandomStreamBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn append_content_from_slice(mut self, content: &[u8]) -> Self {
        self.content.extend(content);
        self
    }

    pub fn append_content_from_iter<I>(mut self, iterator: I) -> Self
    where
        I: IntoIterator<Item = u8>,
    {
        self.content.extend(iterator);
        self
    }

    pub fn append_span_origin(mut self, span_origin: &SpanOrigin) -> Self {
        self.span_origins.push(*span_origin);
        self
    }

    pub fn append_span_origins_from_slice(mut self, span_origins: &[SpanOrigin]) -> Self {
        self.span_origins.extend(span_origins);
        self
    }

    pub fn build(self, rng: &mut ThreadRng) -> BuildRandomStreamResult {
        assert_ne!(self.span_origins.len(), 0);
        assert!(self.content.len() >= self.span_origins.len() - 1);
        let mut splits = Box::<[usize]>::new_uninit_slice(self.span_origins.len() + 1);
        for split in splits.iter_mut() {
            split.write(0);
        }
        // Safe because `splits` was just initialized
        let mut splits = unsafe { splits.assume_init() };
        *splits.last_mut().unwrap() = self.content.len();
        for index in 1..self.span_origins.len() {
            let bytes_left = self.content.len() - splits[index - 1];
            let spans_left = self.span_origins.len() - index - 1;
            let size = rng.random_range(1..=bytes_left - spans_left);
            splits[index] = splits[index - 1] + size;
        }
        let raw_spans: Box<_> = self
            .span_origins
            .iter()
            .enumerate()
            .map(|(index, span_origin)| {
                (
                    &self.content[splits[index]..splits[index + 1]],
                    *span_origin,
                )
            })
            .collect();
        BuildRandomStreamResult {
            stream: LiteralCodeStream::new(&*raw_spans),
            splits,
        }
    }
}

pub fn sample_slice<T, D>(rng: &mut ThreadRng, distribution: D, length: usize) -> Box<[T]>
where
    D: Distribution<T>,
{
    rng.sample_iter(distribution).take(length).collect()
}
