//! This module contains some utilifies that might be useful for testing.

#![allow(dead_code, reason = "tests are not considered for dead code analysis")]

use rand::{Rng, distr::Distribution, rngs::ThreadRng};

pub fn sample_slice<T, D>(rng: &mut ThreadRng, distribution: D, length: usize) -> Box<[T]>
where
    D: Distribution<T>,
{
    rng.sample_iter(distribution).take(length).collect()
}
