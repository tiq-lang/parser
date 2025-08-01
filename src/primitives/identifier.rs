//! This module contains `Identifier` primitive and related parsing functionality and data
//! structures.

use crate::{
    code_stream::{
        core::{CodeSpan, CodeStream, SpanId},
        extensions::{AdvancingRead, StartsWith, TakeWhile},
    },
    error_stack::ErrorStack,
    location::Location,
};

#[derive(Debug, PartialEq)]
pub enum IdentifierKind {
    Common,
    Escaped,
}

/// Represents a semi-valid name for items within the language.
///
/// ## Invariants
/// * `name` must contain a valid ASCII string, that matches the identifier pattern:
/// `[a-zA-Z_][a-zA-Z0-9_]*`.
#[derive(Debug)]
pub struct Identifier {
    kind: IdentifierKind,
    name: Box<[u8]>,
}

/// Metadata for the `Identifier` class. Contains information, that is useful for locating
/// identifier within the source code.
///
/// ## Invariants
/// * `Identifier`s are atoms, which means that entire identifier, as well as optional 'i#' part,
/// must come from a single code span.
/// * `location` always points to the first character of the `name` (even for escaped identifiers).
#[derive(Debug, PartialEq)]
pub struct IdentifierMeta {
    pub(super) location: Location,
    pub(super) span_id: SpanId,
}

impl IdentifierMeta {
    /// Constructs new `IdentifierMeta` instance from `Location` and `SpanId`.
    pub fn new(location: Location, span_id: SpanId) -> Self {
        Self { location, span_id }
    }
}

/// Represents `Identifier` together with associated `IdentifierMeta`.
///
/// `IdentifierWithMeta` inherits all of the invariants of `Identifier` and `IdentifierMeta`.
/// Metadata is hidden behind a `Box` because it is not useful on happy path.
pub struct IdentifierWithMeta {
    identifier: Identifier,
    meta: Box<IdentifierMeta>,
}

impl IdentifierWithMeta {
    pub fn new(identifier: Identifier, meta: IdentifierMeta) -> Self {
        Self {
            identifier,
            meta: Box::new(meta),
        }
    }
}

/// Return result of `Identifier` parsing functions. In case of `ParseResult::Panic` there should
/// be exactly one `ParseError` on the `ErrorStack`.
#[derive(Debug)]
pub enum ParseResult {
    Ok(Identifier, IdentifierMeta),
    Panic,
}

/// Represents errors, that can be pushed onto the `ErrorStack` by `Identifier` parsing functions.
#[derive(Debug)]
pub enum ParseError {
    UnexpectedEndOfStream(Location),
    NonAsciiInitial(Location),
    InvalidAsciiInitial(Location),
}

impl Identifier {
    /// Parse any kind of `Identifier`. Assumes that `stream` points to the first character of
    /// identifier sequence, so all preparations must be done prior to calling this function.
    pub fn parse<S, E>(stream: &mut S, errors: &mut E) -> Result<ParseResult, S::Error>
    where
        S: CodeStream + ?Sized,
        E: ErrorStack + ?Sized,
    {
        if stream.starts_with_mut(b"i#")? {
            match Self::parse_unescaped(stream, errors)? {
                ParseResult::Ok(mut identifier, meta) => {
                    identifier.kind = IdentifierKind::Escaped;
                    Ok(ParseResult::Ok(identifier, meta))
                }
                rest => Ok(rest),
            }
        } else {
            Self::parse_unescaped(stream, errors)
        }
    }

    /// Parses only unescaped identifiers. Behaviour is identical to ordinary `Identifier::parse`,
    /// except there is no special treatment of 'i#' sequence.
    pub fn parse_unescaped<S, E>(stream: &mut S, errors: &mut E) -> Result<ParseResult, S::Error>
    where
        S: CodeStream + ?Sized,
        E: ErrorStack + ?Sized,
    {
        let (span_id, location) = {
            let span_info = stream.get_span_info()?;
            (span_info.span.get_id(), span_info.into())
        };
        let peeked = stream.peek()?;
        if !peeked.is_some_and(|byte| byte.is_ascii_alphabetic() || byte == b'_') {
            errors.push_error(Box::new(match peeked {
                Some(byte) if byte.is_ascii() => ParseError::InvalidAsciiInitial(location),
                Some(_) => ParseError::NonAsciiInitial(location),
                None => ParseError::UnexpectedEndOfStream(location),
            }));
            return Ok(ParseResult::Panic);
        }
        // Safe because `stream.peek()` is `Some`
        let length = unsafe {
            1 + stream.take_while_offset(|byte| byte.is_ascii_alphanumeric() || byte == b'_', 1)?
        };
        let mut identifier = Box::new_uninit_slice(length);
        // Safe because of `take_while_offset` postcondition
        unsafe { stream.advancing_read(&mut *identifier)? }
        Ok(ParseResult::Ok(
            Identifier {
                kind: IdentifierKind::Common,
                // Safe because of `advancing_read` postcondition
                name: unsafe { identifier.assume_init() },
            },
            IdentifierMeta { location, span_id },
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        code_stream::{core::SpanOrigin, literal_code_stream::LiteralCodeStream},
        origin::{self, Leak},
    };
    use std::{path::Path, rc::Rc};

    #[test]
    fn unexpected_end_of_stream() {
        let file: Rc<Path> = Path::new("/file.tiq").into();
        let origin = origin::File(file.clone()).leak();
        let mut stream = LiteralCodeStream::new(&[(
            "",
            SpanOrigin::File {
                file: origin,
                position_in_file: 0,
            },
        )]);
        let mut errors = Vec::new();
        let location: Location = stream.get_span_info().unwrap().into();
        let parse_result = Identifier::parse(&mut stream, &mut errors).unwrap();
        assert!(matches!(parse_result, ParseResult::Panic));
        let error = errors
            .inspect_last_error()
            .unwrap()
            .downcast_ref::<ParseError>()
            .unwrap();
        match error {
            ParseError::UnexpectedEndOfStream(error_location) => {
                assert_eq!(*error_location, location);
            }
            rest => panic!("unexpected `error` type: {:?}", rest),
        }
    }

    #[test]
    fn non_ascii_initial() {
        let file: Rc<Path> = Path::new("/file.tiq").into();
        let origin = origin::File(file.clone()).leak();
        let mut stream = LiteralCodeStream::new(&[(
            "😊",
            SpanOrigin::File {
                file: origin,
                position_in_file: 0,
            },
        )]);
        let mut errors = Vec::new();
        let location: Location = stream.get_span_info().unwrap().into();
        let parse_result = Identifier::parse(&mut stream, &mut errors).unwrap();
        assert!(matches!(parse_result, ParseResult::Panic));
        let error = errors
            .inspect_last_error()
            .unwrap()
            .downcast_ref::<ParseError>()
            .unwrap();
        match error {
            ParseError::NonAsciiInitial(error_location) => {
                assert_eq!(*error_location, location);
            }
            rest => panic!("unexpected `error` type: {:?}", rest),
        }
    }

    #[test]
    fn invalid_ascii_initial() {
        let file: Rc<Path> = Path::new("/file.tiq").into();
        let origin = origin::File(file.clone()).leak();
        let mut stream = LiteralCodeStream::new(&[(
            "$",
            SpanOrigin::File {
                file: origin,
                position_in_file: 0,
            },
        )]);
        let mut errors = Vec::new();
        let location: Location = stream.get_span_info().unwrap().into();
        let parse_result = Identifier::parse(&mut stream, &mut errors).unwrap();
        assert!(matches!(parse_result, ParseResult::Panic));
        let error = errors
            .inspect_last_error()
            .unwrap()
            .downcast_ref::<ParseError>()
            .unwrap();
        match error {
            ParseError::InvalidAsciiInitial(error_location) => {
                assert_eq!(*error_location, location);
            }
            rest => panic!("unexpected `error` type: {:?}", rest),
        }
    }

    fn check_unescaped_identifier(identifier_string: &str, span_origin: SpanOrigin) {
        let mut stream = LiteralCodeStream::new(&[(identifier_string, span_origin)]);
        let mut errors = Vec::new();
        let (span_id, location): (_, Location) = {
            let span_info = stream.get_span_info().unwrap();
            (span_info.span.get_id(), span_info.into())
        };
        match Identifier::parse(&mut stream, &mut errors).unwrap() {
            ParseResult::Ok(identifier, meta) => {
                assert_eq!(identifier.kind, IdentifierKind::Common);
                assert_eq!(*identifier.name, *identifier_string.as_bytes());
                assert_eq!(meta, IdentifierMeta { location, span_id });
            }
            ParseResult::Panic => panic!("unexpected parsing failure"),
        }
    }

    #[test]
    fn keywords() {
        const SAMPLES: [&str; 8] = ["unsafe", "mut", "drop", "self", "trait", "impl", "as", "is"];

        let file: Rc<Path> = Path::new("/file.tiq").into();
        let origin = origin::File(file.clone()).leak();
        for &sample in SAMPLES.iter() {
            check_unescaped_identifier(
                sample,
                SpanOrigin::File {
                    file: origin,
                    position_in_file: 0,
                },
            );
        }
    }

    #[test]
    fn underscores() {
        const SAMPLES: [&str; 4] = ["_", "_id", "snake_case", "error_location"];

        let file: Rc<Path> = Path::new("/file.tiq").into();
        let origin = origin::File(file.clone()).leak();
        for &sample in SAMPLES.iter() {
            check_unescaped_identifier(
                sample,
                SpanOrigin::File {
                    file: origin,
                    position_in_file: 0,
                },
            );
        }
    }
}
