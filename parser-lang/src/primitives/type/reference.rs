//! TODO: write docs

use super::{ParseError, ParseResult, Type};
use crate::byte_stream::extensions::{StartsWithIdent, TakeWhitespaces};
use parser_core::{
    byte_stream::core::{ByteStream, Position},
    error_stack::ErrorStack,
};
use std::num::NonZero;

/// TODO: write docs
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PointerKind {
    Ref,
    Ptr,
}

/// TODO: write docs
#[derive(Debug, PartialEq)]
pub enum PointerQualifier {
    Mut(Position),
    Drop(Position),
}

/// TODO: write docs
#[derive(Debug, PartialEq)]
pub struct Ref {
    pub(crate) pointee: Box<Type>,
    pub(crate) qualifier: Option<PointerQualifier>,
    pub(crate) position: Position,
}

/// TODO: write docs
#[derive(Debug, PartialEq)]
pub struct Ptr {
    pub(crate) pointee: Box<Type>,
    pub(crate) qualifier: Option<PointerQualifier>,
    pub(crate) position: Position,
}

impl PointerQualifier {
    /// TODO: write docs
    fn parse<S>(stream: &mut S) -> Result<Option<Self>, S::Error>
    where
        S: ByteStream + ?Sized,
    {
        stream.take_whitespaces_mut()?;
        let qualifier = if stream.starts_with_ident(b"mut")? {
            let position = stream.get_position()?;
            // Safe because of `starts_with_ident` postcondition
            unsafe { stream.advance_by(NonZero::new_unchecked(3))? }
            Some(Self::Mut(position))
        } else if stream.starts_with_ident(b"drop")? {
            let position = stream.get_position()?;
            // Safe because of `starts_with_ident` postcondition
            unsafe { stream.advance_by(NonZero::new_unchecked(4))? }
            Some(Self::Drop(position))
        } else {
            None
        };
        if qualifier.is_some() {
            stream.take_whitespaces_mut()?;
        }
        Ok(qualifier)
    }
}

impl Ref {
    /// TODO: write docs
    pub fn parse<S, E>(stream: &mut S, errors: &mut E) -> Result<ParseResult<Self>, S::Error>
    where
        S: ByteStream + ?Sized,
        E: ErrorStack + ?Sized,
    {
        let ref_position = stream
            .get_position()?
            .offset(-1)
            .expect("`Ref::parse` should be called after '&' was read");
        let ref_qualifier = PointerQualifier::parse(stream)?;
        if let Err(error) = Type::check_initial(stream.peek()?) {
            errors.push_erased_error(Box::new(ParseError::PointeeInitial {
                pointer_kind: PointerKind::Ref,
                pointer_position: ref_position,
                pointer_qualifier: ref_qualifier,
                error: (error, stream.get_position()?),
            }));
            return Ok(ParseResult::Panic);
        }
        Ok(Type::parse(stream, errors)?.map(|pointee| Self {
            pointee: Box::new(pointee),
            qualifier: ref_qualifier,
            position: ref_position,
        }))
    }
}

impl Ptr {
    /// TODO: write docs
    pub fn parse<S, E>(stream: &mut S, errors: &mut E) -> Result<ParseResult<Self>, S::Error>
    where
        S: ByteStream + ?Sized,
        E: ErrorStack + ?Sized,
    {
        let ptr_position = stream
            .get_position()?
            .offset(-1)
            .expect("`Ptr::parse` should be called after '*' was read");
        let qualifier = PointerQualifier::parse(stream)?;
        if let Err(error) = Type::check_initial(stream.peek()?) {
            errors.push_erased_error(Box::new(ParseError::PointeeInitial {
                pointer_kind: PointerKind::Ptr,
                pointer_position: ptr_position,
                pointer_qualifier: qualifier,
                error: (error, stream.get_position()?),
            }));
            return Ok(ParseResult::Panic);
        }
        Ok(Type::parse(stream, errors)?.map(|pointee| Self {
            pointee: Box::new(pointee),
            qualifier,
            position: ptr_position,
        }))
    }
}
