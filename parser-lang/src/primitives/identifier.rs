//! This module contains `Identifier` primitive and related parsing functionality and data
//! structures.

use parser_core::{
    byte_stream::{
        core::{ByteStream, Position},
        extensions::{AdvancingRead, StartsWith, TakeWhile},
    },
    error_stack::ErrorStack,
};
use std::mem::{MaybeUninit, transmute};

pub mod reserved_word;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum IdentifierKind {
    Common,
    Escaped,
}

/// Represents a semi-valid name (modulo reserved words) for items within the language.
///
/// `Identifier` utilizes small buffer optimization to reduce heap allocations. Inner constant
/// `SMALL_THRESHOLD` provides upper limit for length of identifiers, that are stored on the stack.
///
/// ## Invariants
/// * `Identifier::as_ref` returns a valid ASCII string, that matches the identifier pattern:
/// `[a-zA-Z_][a-zA-Z0-9_]*`.
#[derive(Debug)]
pub enum Identifier {
    CommonSmall {
        size: u8,
        buffer: [u8; Self::SMALL_THRESHOLD as usize],
    },
    Common {
        bytes: Box<[u8]>,
    },
    EscapedSmall {
        size: u8,
        buffer: [u8; Self::SMALL_THRESHOLD as usize],
    },
    Escaped {
        bytes: Box<[u8]>,
    },
}

impl Identifier {
    /// Upper limit for length of identifiers which utilize small buffer optimization.
    const SMALL_THRESHOLD: u8 = 22;

    /// Consumes `Identifier` and changes its kind to `IdentifierKind::Escaped`.
    pub(crate) fn to_escaped(self) -> Self {
        match self {
            Self::CommonSmall { size, buffer } => Self::EscapedSmall { size, buffer },
            Self::Common { bytes } => Self::Escaped { bytes },
            rest => rest,
        }
    }

    /// Checks whether given instance of `Identifier` utilizes small buffer optimization.
    #[allow(dead_code)] // used within tests
    pub(crate) fn uses_small_optimization(&self) -> bool {
        match self {
            Self::CommonSmall { .. } | Self::EscapedSmall { .. } => true,
            Self::Common { .. } | Self::Escaped { .. } => false,
        }
    }

    /// Determines kind of provided `Identifier` instance.
    pub fn get_kind(&self) -> IdentifierKind {
        match self {
            Self::Common { .. } | Self::CommonSmall { .. } => IdentifierKind::Common,
            Self::Escaped { .. } | Self::EscapedSmall { .. } => IdentifierKind::Escaped,
        }
    }
}

impl AsRef<[u8]> for Identifier {
    fn as_ref(&self) -> &[u8] {
        match self {
            &Identifier::CommonSmall { size, ref buffer } => &buffer[..size as usize],
            Identifier::Common { bytes } => bytes,
            &Identifier::EscapedSmall { size, ref buffer } => &buffer[..size as usize],
            Identifier::Escaped { bytes } => bytes,
        }
    }
}

impl PartialEq<(IdentifierKind, &[u8])> for Identifier {
    fn eq(&self, other: &(IdentifierKind, &[u8])) -> bool {
        self.get_kind() == other.0 && self.as_ref() == other.1
    }
}

/// Metadata for the `Identifier` class. Contains information, that is useful for locating
/// identifier within the source code.
///
/// ## Invariants
/// * `Identifier`s are atoms, which means that entire identifier, as well as optional 'i#' part,
/// must come from a single code span.
/// * `location` always points to the first character of the identifier (even for escaped
/// identifiers).
#[derive(Debug, PartialEq)]
pub struct IdentifierMeta(Position);

impl IdentifierMeta {
    /// Constructs new `IdentifierMeta` instance from `Position`.
    pub(crate) fn new(position: Position) -> Self {
        Self(position)
    }
}

/// Represents `Identifier` together with associated `IdentifierMeta`.
///
/// `IdentifierWithMeta` inherits all of the invariants of `Identifier` and `IdentifierMeta`.
#[derive(Debug)]
pub struct IdentifierWithMeta {
    identifier: Identifier,
    meta: IdentifierMeta,
}

impl IdentifierWithMeta {
    /// Constructs new `IdentifierWithMeta` instance from `Identifier` and `IdentifierMeta`.
    pub fn new(identifier: Identifier, meta: IdentifierMeta) -> Self {
        Self {
            identifier,
            meta: meta,
        }
    }
}

/// Return type of `Identifier` parsing functions.
#[derive(Debug)]
pub enum ParseResult {
    Ok(Identifier, IdentifierMeta),
    Panic,
}

/// Error type of `Identifier` parsing functions.
#[derive(Debug)]
pub enum ParseError {
    UnexpectedEndOfStream(Position),
    NonAsciiInitial(Position),
    InvalidAsciiInitial(Position),
}

impl Identifier {
    /// Parse any kind of `Identifier`. Assumes that `stream` points to the first character of
    /// identifier sequence. If this returns `ParseResult::Panic` one `ParseError` is pushed onto
    /// error stack.
    pub fn parse<S, E>(stream: &mut S, errors: &mut E) -> Result<ParseResult, S::Error>
    where
        S: ByteStream + ?Sized,
        E: ErrorStack + ?Sized,
    {
        if stream.starts_with_mut(b"i#")? {
            match Self::parse_unescaped(stream, errors)? {
                ParseResult::Ok(identifier, meta) => {
                    Ok(ParseResult::Ok(identifier.to_escaped(), meta))
                }
                ParseResult::Panic => Ok(ParseResult::Panic),
            }
        } else {
            Self::parse_unescaped(stream, errors)
        }
    }

    /// Parses only unescaped identifiers. Behaviour is identical to ordinary `Identifier::parse`,
    /// except there is no special treatment of 'i#' sequence. If this returns `ParseResult::Panic`
    /// one `ParseError` is pushed onto error stack.
    pub fn parse_unescaped<S, E>(stream: &mut S, errors: &mut E) -> Result<ParseResult, S::Error>
    where
        S: ByteStream + ?Sized,
        E: ErrorStack + ?Sized,
    {
        let position = stream.get_position()?;
        let peeked = stream.peek()?;
        if !peeked.is_some_and(|byte| byte.is_ascii_alphabetic() || byte == b'_') {
            errors.push_error(Box::new(match peeked {
                Some(byte) if byte.is_ascii() => ParseError::InvalidAsciiInitial(position),
                Some(_) => ParseError::NonAsciiInitial(position),
                None => ParseError::UnexpectedEndOfStream(position),
            }));
            return Ok(ParseResult::Panic);
        }
        // Safe because `stream.peek()` is `Some`
        let length = unsafe {
            1 + stream.take_while_offset(|byte| byte.is_ascii_alphanumeric() || byte == b'_', 1)?
        };
        if length <= Self::SMALL_THRESHOLD as usize {
            let mut buffer = [MaybeUninit::new(0); Self::SMALL_THRESHOLD as usize];
            // Safe because of `take_while` postcondition
            unsafe { stream.advancing_read(&mut buffer[..length])? };
            Ok(ParseResult::Ok(
                Identifier::CommonSmall {
                    size: length as u8,
                    // Safe because `buffer` is never uninitialized
                    buffer: unsafe { transmute(buffer) },
                },
                IdentifierMeta(position),
            ))
        } else {
            let mut identifier = Box::new_uninit_slice(length);
            // Safe because of `take_while_offset` postcondition
            unsafe { stream.advancing_read(&mut *identifier)? }
            Ok(ParseResult::Ok(
                Identifier::Common {
                    // Safe because of `advancing_read` postcondition
                    bytes: unsafe { identifier.assume_init() },
                },
                IdentifierMeta(position),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use self::CheckIdentifierArguments as Arguments;
    use super::*;
    use parser_core::byte_stream::literal_byte_stream::LiteralByteStream;

    pub struct CheckIdentifierArguments<'a> {
        pub identifier_kind: IdentifierKind,
        /// This shouldn't include 'i#' part even if `identifier_kind` is `IdentifierKind::Escaped`.
        pub identifier: &'a [u8],
    }

    fn check_identifier(arguments: Arguments) {
        let Arguments {
            identifier_kind,
            identifier,
        } = arguments;
        let (mut stream, position) = if identifier_kind == IdentifierKind::Escaped {
            (
                LiteralByteStream::new(
                    [b"i#", identifier].into_iter().flatten().copied().collect(),
                ),
                Position::new(2),
            )
        } else {
            (LiteralByteStream::new(identifier.into()), Position::new(0))
        };
        let mut errors = Vec::new();
        match Identifier::parse(&mut stream, &mut errors).unwrap() {
            ParseResult::Ok(parsed_identifier, meta) => {
                assert_eq!(parsed_identifier, (identifier_kind, identifier));
                assert_eq!(meta, IdentifierMeta(position));
                assert_eq!(
                    parsed_identifier.uses_small_optimization(),
                    identifier.len() <= Identifier::SMALL_THRESHOLD as usize
                );
            }
            ParseResult::Panic => panic!("unexpected parsing failure"),
        }
    }

    #[test]
    fn unexpected_end_of_stream() {
        let mut stream = LiteralByteStream::new("".as_bytes().into());
        let mut errors = Vec::new();
        let position = stream.get_position().unwrap();
        let parse_result = Identifier::parse(&mut stream, &mut errors).unwrap();
        assert!(matches!(parse_result, ParseResult::Panic));
        let error = errors
            .inspect_last_error()
            .unwrap()
            .downcast_ref::<ParseError>()
            .unwrap();
        match error {
            ParseError::UnexpectedEndOfStream(error_position) => {
                assert_eq!(*error_position, position);
            }
            rest => panic!("unexpected `error` type: {:?}", rest),
        }
    }

    #[test]
    fn non_ascii_initial() {
        let mut stream = LiteralByteStream::new("😊".as_bytes().into());
        let mut errors = Vec::new();
        let position = stream.get_position().unwrap();
        let parse_result = Identifier::parse(&mut stream, &mut errors).unwrap();
        assert!(matches!(parse_result, ParseResult::Panic));
        let error = errors
            .inspect_last_error()
            .unwrap()
            .downcast_ref::<ParseError>()
            .unwrap();
        match error {
            ParseError::NonAsciiInitial(error_position) => {
                assert_eq!(*error_position, position);
            }
            rest => panic!("unexpected `error` type: {:?}", rest),
        }
    }

    #[test]
    fn invalid_ascii_initial() {
        let mut stream = LiteralByteStream::new("$".as_bytes().into());
        let mut errors = Vec::new();
        let position = stream.get_position().unwrap();
        let parse_result = Identifier::parse(&mut stream, &mut errors).unwrap();
        assert!(matches!(parse_result, ParseResult::Panic));
        let error = errors
            .inspect_last_error()
            .unwrap()
            .downcast_ref::<ParseError>()
            .unwrap();
        match error {
            ParseError::InvalidAsciiInitial(error_position) => {
                assert_eq!(*error_position, position);
            }
            rest => panic!("unexpected `error` type: {:?}", rest),
        }
    }

    #[test]
    fn keywords() {
        const SAMPLES: [&str; 8] = ["unsafe", "mut", "drop", "self", "trait", "impl", "as", "is"];

        for &sample in SAMPLES.iter() {
            check_identifier(CheckIdentifierArguments {
                identifier_kind: IdentifierKind::Common,
                identifier: sample.as_bytes(),
            });
        }
    }

    #[test]
    fn underscores() {
        const SAMPLES: [&str; 4] = ["_", "_id", "snake_case", "error_location"];

        for &sample in SAMPLES.iter() {
            check_identifier(CheckIdentifierArguments {
                identifier_kind: IdentifierKind::Common,
                identifier: sample.as_bytes(),
            });
        }
    }

    #[test]
    fn escaped() {
        const SAMPLES: [&str; 4] = ["as", "struct", "use", "Self"];

        for &sample in SAMPLES.iter() {
            check_identifier(CheckIdentifierArguments {
                identifier_kind: IdentifierKind::Escaped,
                identifier: sample.as_bytes(),
            });
        }
    }

    #[test]
    fn small_optimization() {
        const SAMPLES: [&str; 2] = ["optimized_identifier__", "unoptimized_identifier_"];

        for &sample in SAMPLES.iter() {
            check_identifier(CheckIdentifierArguments {
                identifier_kind: IdentifierKind::Escaped,
                identifier: sample.as_bytes(),
            });
        }
    }
}
