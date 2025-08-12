//! This module contains various sources from which the source code (or bytes in general) can
//! be read by the compiler.
//!
//! Currently there are 4 available sources. `File` represents bytes, that actually exist in
//! some file, which means that we can point to them. Unfortunately, not all bytes are created
//! equal and are so easy to deal with. Some bytes are *synthetic*, which means that they are
//! created during compilation and do not exist naturally.
//!
//! Synthetic sources are:
//!   * `Macro` - bytes that come from an invocation of inline or top-level macro.
//!   * `Derive` - bytes that come from the `derive` attribute on data types.
//!   * `Attribute` - bytes that come from invocation of arbitrary attribute.
//!
//! More specific information about sources can be found on their respective pages.

use std::{ops::Range, path::Path, rc::Rc};

/// Denotes bytes that come from a file.
///
/// ## Invariants
/// * Contained `Rc` must point to absolute path, that denotes some existing file in the filesystem.
#[derive(Debug, PartialEq)]
pub struct File(pub Rc<Path>);

/// Denotes bytes that come from an invocation of inline or top-level macro.
///
/// Contained `Range` points to the range of bytes in the aliased file, that contain the macro head
/// (e.g. `my::macro!` in `my::macro!(...)`).
///
/// ## Invariants
/// * Contained `Rc` must point to absolute path, that denotes some existing file in the filesystem.
#[derive(Debug, PartialEq)]
pub struct Macro(pub Rc<Path>, pub Range<usize>);

/// Denotes bytes that come from the `derive` attribute on data types.
///
/// Contained `Range` points to the bytes in the aliased file, that contain the `derive`
/// attribute argument (e.g. `Debug` in `#[derive(Debug)]`).
///
/// ## Invariants
/// * Contained `Rc` must point to absolute path, that denotes some existing file in the filesystem.
#[derive(Debug, PartialEq)]
pub struct Derive(pub Rc<Path>, pub Range<usize>);

/// Denotes bytes that come from invocation of arbitrary attribute.
///
/// Contained `Range` points to the bytes in the aliased file, that contain the attrribute head
/// (e.g. `attr` in `#[attr(...)]`).
///
/// ## Invariants
/// * Contained `Rc` must point to absolute path, that denotes some existing file in the filesystem.
#[derive(Debug, PartialEq)]
pub struct Attribute(pub Rc<Path>, pub Range<usize>);

/// Helper trait, that simplifies creation of static references at runtime.
pub trait Leak: Sized {
    fn leak(self) -> &'static Self {
        Box::leak(Box::new(self))
    }
}

impl Leak for File {}
impl Leak for Macro {}
impl Leak for Derive {}
impl Leak for Attribute {}
