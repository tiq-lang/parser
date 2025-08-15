//! This module contains some utilifies that might be useful for testing.

// NOTE: rust-analyzer doesn't consider tests for dead code analysis for some reason
#![allow(dead_code)]

use rand::{Rng, distr::Distribution, rngs::ThreadRng};

pub fn sample_slice<T, D>(rng: &mut ThreadRng, distribution: D, length: usize) -> Box<[T]>
where
    D: Distribution<T>,
{
    rng.sample_iter(distribution).take(length).collect()
}
