//! This module contains `ErrorStack` trait and related utilities to abstract and simplify error
//! handling and propagation.

use std::{
    any::Any,
    mem::{MaybeUninit, replace, transmute},
    num::NonZero,
    slice,
};

/// Abstracts away methods of pushing and inspecting errors using the stack-like interface.
pub trait ErrorStack {
    /// Type of the inspect iterator of the stack.
    type Iterator<'a>: Iterator<Item = &'a mut Box<dyn Any>>
    where
        Self: 'a;

    /// Push type-erased error onto the stack.
    fn push_error(&mut self, error: Box<dyn Any>);

    /// Produces an iterator over the last `amount` errors pushed onto the stack. Returns `None`
    /// only if `amount` is larger than the size of a stack.
    fn inspect_errors(&mut self, amount: NonZero<usize>) -> Option<Self::Iterator<'_>>;

    /// Helper method to inspect only the last error. Returns `None` only if the stack is empty.
    fn inspect_last_error(&mut self) -> Option<&mut Box<dyn Any>> {
        Some(
            self.inspect_errors(unsafe { NonZero::new_unchecked(1) })?
                .next()
                .expect("`inspect_error` should yield iterator with one element"),
        )
    }
}

impl ErrorStack for Vec<Box<dyn Any>> {
    type Iterator<'a>
        = slice::IterMut<'a, Box<dyn Any>>
    where
        Self: 'a;

    fn push_error(&mut self, error: Box<dyn Any>) {
        self.push(error);
    }

    fn inspect_errors<'a>(&'a mut self, amount: NonZero<usize>) -> Option<Self::Iterator<'a>> {
        let index = self.len().checked_sub(amount.get())?;
        Some(self.get_mut(index..)?.iter_mut())
    }
}

/// Replaces specified memory location with the pointer to uninitialized memory.
///
/// ## Safety
/// For this function to be safe resulting reference must be assigned a valid value of `T`,
/// otherwise this function results in a UB.
pub unsafe fn replace_with_uninit<'a, T: 'static>(
    destination: &'a mut Box<dyn Any>,
) -> (Box<dyn Any>, &'a mut MaybeUninit<T>) {
    let ptr = Box::into_raw(Box::<T>::new_uninit());
    (
        replace(destination, unsafe {
            Box::from_raw(transmute::<_, *mut T>(ptr))
        }),
        unsafe { &mut *ptr },
    )
}
