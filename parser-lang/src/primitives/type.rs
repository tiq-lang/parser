//! TODO: write docs

use self::{
    reference::{PointerKind, PointerQualifier, Ptr, Ref},
    tuple::{Grouped, Tuple},
};
use crate::primitives::{
    identifier::{
        Identifier,
        reserved_word::{self, IdentifierOrReservedWord, ReservedWord},
    },
    r#type::tuple::TupleParser,
};
use derive_more::From;
use parser_core::{
    byte_stream::core::{ByteStream, Position},
    error_stack::ErrorStack,
};
use std::num::NonZero;

pub mod reference;
pub mod tuple;

/// TODO: write docs
#[derive(Debug, PartialEq)]
pub struct Placeholder {
    position: Position,
}

/// TODO: write docs
#[derive(Debug, PartialEq)]
pub struct Never {
    position: Position,
}

#[derive(Debug, From, PartialEq)]
pub enum Type {
    Placeholder(Placeholder),
    Never(Never),
    Ref(Ref),
    Ptr(Ptr),
    Tuple(Tuple),
    Grouped(Grouped),
    Slice(),
    Fn(),
    // TODO: should be replaced with qualified path when ready
    Named(Identifier),
    // TODO: add fixed-length arrays, opaque (impl Trait) types, dyn (dyn Trait) types
}

/// Return type of `Type` parsing functions.
#[derive(Debug)]
pub enum ParseResult<T> {
    Ok(T),
    None,
    Panic,
}

impl<T> ParseResult<T> {
    pub fn map<M, U>(self, mapping: M) -> ParseResult<U>
    where
        M: FnOnce(T) -> U,
    {
        match self {
            ParseResult::Ok(value) => ParseResult::Ok(mapping(value)),
            ParseResult::None => ParseResult::None,
            ParseResult::Panic => ParseResult::Panic,
        }
    }
}

/// Error type of `Type` and all related parsing functions.
#[derive(Debug)]
pub enum ParseError {
    Initial(InitialParseError, Position),
    PointeeInitial {
        pointer_kind: PointerKind,
        pointer_position: Position,
        pointer_qualifier: Option<PointerQualifier>,
        error: (InitialParseError, Position),
    },
    TupleOrGroupedInitial {
        opening_brace_position: Position,
        error: (InitialParseError, Position),
    },
    TupleOrGroupedUnexpectedSeparator {
        // TODO: think of a better name (pre_whitespace_position?)
        preposition: Position,
        error_position: Position,
    },
    TupleInitial {
        preposition: Position,
        error: (InitialParseError, Position),
    },
}

/// TODO: write docs
#[derive(Debug)]
pub enum InitialParseError {
    UnexpectedEndOfStream,
    NonAsciiInitial,
    InvalidAsciiInitial,
}

impl Type {
    /// TODO: write docs
    fn check_initial(byte: Option<u8>) -> Result<(), InitialParseError> {
        match byte {
            Some(b'!') | Some(b'&') | Some(b'*') | Some(b'(') | Some(b'[') | Some(b'<') => Ok(()),
            Some(byte) if byte.is_ascii_alphabetic() || byte == b'_' => Ok(()),
            rest => Err(match rest {
                Some(byte) if byte.is_ascii() => InitialParseError::InvalidAsciiInitial,
                Some(_) => InitialParseError::NonAsciiInitial,
                None => InitialParseError::UnexpectedEndOfStream,
            }),
        }
    }

    /// TODO: write docs
    pub fn parse<S, E>(stream: &mut S, errors: &mut E) -> Result<ParseResult<Self>, S::Error>
    where
        S: ByteStream + ?Sized,
        E: ErrorStack + ?Sized,
    {
        match stream.peek()? {
            Some(b'!') => {
                let r#type = Never {
                    position: stream.get_position()?,
                };
                // Safe because `stream.peek()` is `Some`
                unsafe { stream.advance_by(NonZero::new_unchecked(1))? };
                Ok(ParseResult::Ok(Type::Never(r#type)))
            }
            Some(b'&') => {
                // Safe because `stream.peek()` is `Some`
                unsafe { stream.advance_by(NonZero::new_unchecked(1))? }
                Ok(Ref::parse(stream, errors)?.map(|reference| Self::Ref(reference)))
            }
            Some(b'*') => {
                // Safe because `stream.peek()` is `Some`
                unsafe { stream.advance_by(NonZero::new_unchecked(1))? }
                Ok(Ptr::parse(stream, errors)?.map(|pointer| Self::Ptr(pointer)))
            }
            // TODO: slice or array types
            Some(b'[') => todo!(),
            Some(b'(') => {
                // Safe because `stream.peek()` is `Some`
                unsafe { stream.advance_by(NonZero::new_unchecked(1))? }
                Ok(TupleParser::new(stream, errors)
                    .parse()?
                    .map(|r#type| match r#type {
                        tuple::TupleOrGrouped::Tuple(tuple) => Self::Tuple(tuple),
                        tuple::TupleOrGrouped::Grouped(grouped) => Self::Grouped(grouped),
                    }))
            }
            // TODO: qualified paths
            Some(b'<') => todo!(),
            Some(byte) if byte.is_ascii_alphabetic() || byte == b'_' => {
                match IdentifierOrReservedWord::parse(stream, errors)? {
                    reserved_word::ParseResult::Ok(IdentifierOrReservedWord::Identifier(
                        identifier,
                    )) => Ok(ParseResult::Ok(Self::Named(identifier))),
                    reserved_word::ParseResult::Ok(IdentifierOrReservedWord::ReservedWord(
                        word,
                        position,
                    )) => match word {
                        ReservedWord::Wildcard => {
                            Ok(ParseResult::Ok(Self::Placeholder(Placeholder { position })))
                        }
                        // TODO: function pointer types
                        ReservedWord::Fn => todo!(),
                        // TODO: opaque trait types
                        ReservedWord::Impl => todo!(),
                        _ => todo!(),
                    },
                    reserved_word::ParseResult::Panic => Ok(ParseResult::Panic),
                }
            }
            rest => {
                errors.push_erased_error(Box::new(ParseError::Initial(
                    match rest {
                        Some(byte) if byte.is_ascii() => InitialParseError::InvalidAsciiInitial,
                        Some(_) => InitialParseError::NonAsciiInitial,
                        None => InitialParseError::UnexpectedEndOfStream,
                    },
                    stream.get_position()?,
                )));
                Ok(ParseResult::Panic)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser_core::byte_stream::literal_byte_stream::LiteralByteStream;

    struct CheckTypeArguments<'a> {
        bytes: &'a [u8],
        expected_type: Type,
    }

    fn check_type(arguments: CheckTypeArguments) {
        let CheckTypeArguments {
            bytes,
            expected_type,
        } = arguments;
        let mut stream = LiteralByteStream::new(bytes.into());
        let mut errors = Vec::new();
        let parsed_type = match Type::parse(&mut stream, &mut errors).unwrap() {
            ParseResult::Ok(r#type) => r#type,
            rest => panic!("unexpected parsing failure: {:?}", rest),
        };
        assert_eq!(parsed_type, expected_type);
    }

    #[test]
    fn placeholder() {
        check_type(CheckTypeArguments {
            bytes: b"_",
            expected_type: Placeholder {
                position: Position::new(0),
            }
            .into(),
        });
    }

    #[test]
    fn never() {
        check_type(CheckTypeArguments {
            bytes: b"!",
            expected_type: Never {
                position: Position::new(0),
            }
            .into(),
        });
    }

    #[test]
    fn named() {
        check_type(CheckTypeArguments {
            bytes: b"i32",
            expected_type: Identifier::new(b"i32", Position::new(0)).into(),
        });
    }

    #[test]
    fn reference() {
        check_type(CheckTypeArguments {
            bytes: b"&_",
            expected_type: Ref {
                pointee: Box::new(
                    Placeholder {
                        position: Position::new(1),
                    }
                    .into(),
                ),
                qualifier: None,
                position: Position::new(0),
            }
            .into(),
        });
    }

    #[test]
    fn mut_reference() {
        check_type(CheckTypeArguments {
            bytes: b"& mut !",
            expected_type: Ref {
                pointee: Box::new(
                    Never {
                        position: Position::new(6),
                    }
                    .into(),
                ),
                qualifier: Some(PointerQualifier::Mut(Position::new(2))),
                position: Position::new(0),
            }
            .into(),
        });
    }

    #[test]
    fn drop_reference() {
        check_type(CheckTypeArguments {
            bytes: b"&drop _",
            expected_type: Ref {
                pointee: Box::new(
                    Placeholder {
                        position: Position::new(6),
                    }
                    .into(),
                ),
                qualifier: Some(PointerQualifier::Drop(Position::new(1))),
                position: Position::new(0),
            }
            .into(),
        });
    }

    #[test]
    fn tuple() {
        check_type(CheckTypeArguments {
            bytes: b"(_, _, )",
            expected_type: Tuple {
                elements: [
                    Placeholder {
                        position: Position::new(1),
                    }
                    .into(),
                    Placeholder {
                        position: Position::new(4),
                    }
                    .into(),
                ]
                .into(),
                brace_positions: [Position::new(0), Position::new(7)],
            }
            .into(),
        });
    }

    #[test]
    fn grouped() {
        check_type(CheckTypeArguments {
            bytes: b"(&mut usize)",
            expected_type: Grouped {
                inner: Box::new(
                    Ref {
                        pointee: Box::new(Identifier::new(b"usize", Position::new(6)).into()),
                        qualifier: Some(PointerQualifier::Mut(Position::new(2))),
                        position: Position::new(1),
                    }
                    .into(),
                ),
                brace_positions: [Position::new(0), Position::new(11)],
            }
            .into(),
        });
    }
}
