//! This module contains `ReservedWord` enum and extension for identifier parsing, which
//! distinguishes identifiers and reserved words.

use super::{Identifier, IdentifierKind};
use crate::primitives::identifier;
use parser_core::{
    byte_stream::core::{ByteStream, Position},
    error_stack::ErrorStack,
};
use phf::phf_map;
use strum::EnumIter;

/// Enumeration with all keywords, special identifiers and identifiers reserved for future use.
#[derive(Clone, Copy, Debug, EnumIter, PartialEq)]
pub enum ReservedWord {
    /// Written as `pub`
    Pub,
    /// Written as `use`
    Use,
    /// Written as `self`
    SelfLowercase,
    /// Written as `super`
    Super,
    /// Written as `blob`
    Blob,

    /// Written as `struct`
    Struct,
    /// Written as `enum`
    Enum,
    /// Written as `trait`
    Trait,
    /// Written as `fn`
    Fn,
    /// Written as `const`
    Const,
    /// Written as `static`
    Static,
    /// Written as `impl`
    Impl,

    /// Written as `as`
    As,
    /// Written as `is`
    Is,
    /// Written as `let`
    Let,
    /// Written as `if`
    If,
    /// Written as `else`
    Else,
    /// Written as `match`
    Match,
    /// Written as `loop`
    Loop,
    /// Written as `while`
    While,
    /// Written as `for`
    For,
    /// Written as `break`
    Break,
    /// Written as `continue`
    Continue,
    /// Written as `return`
    Return,

    /// Written as `where`
    Where,
    /// Written as `Self`
    SelfUppercase,

    /// Written as `unsafe`
    Unsafe,
    /// Written as `_`
    Wildcard,
}

impl From<ReservedWord> for &'static str {
    fn from(value: ReservedWord) -> Self {
        match value {
            ReservedWord::Pub => "pub",
            ReservedWord::Use => "use",
            ReservedWord::SelfLowercase => "self",
            ReservedWord::Super => "super",
            ReservedWord::Blob => "blob",
            ReservedWord::Struct => "struct",
            ReservedWord::Enum => "enum",
            ReservedWord::Trait => "trait",
            ReservedWord::Fn => "fn",
            ReservedWord::Const => "const",
            ReservedWord::Static => "static",
            ReservedWord::Impl => "impl",
            ReservedWord::As => "as",
            ReservedWord::Is => "is",
            ReservedWord::Let => "let",
            ReservedWord::If => "if",
            ReservedWord::Else => "else",
            ReservedWord::Match => "match",
            ReservedWord::Loop => "loop",
            ReservedWord::While => "while",
            ReservedWord::For => "for",
            ReservedWord::Break => "break",
            ReservedWord::Continue => "continue",
            ReservedWord::Return => "return",
            ReservedWord::Where => "where",
            ReservedWord::SelfUppercase => "Self",
            ReservedWord::Unsafe => "unsafe",
            ReservedWord::Wildcard => "_",
        }
    }
}

impl TryFrom<&[u8]> for ReservedWord {
    type Error = ();

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        match RESERVED_WORDS.get(value) {
            Some(&reserved_word) => Ok(reserved_word),
            None => Err(()),
        }
    }
}

/// Mapping from byte slices to reserved words.
static RESERVED_WORDS: phf::Map<&'static [u8], ReservedWord> = phf_map! {
    b"pub" => ReservedWord::Pub,
    b"use" => ReservedWord::Use,
    b"self" => ReservedWord::SelfLowercase,
    b"super" => ReservedWord::Super,
    b"blob" => ReservedWord::Blob,
    b"struct" => ReservedWord::Struct,
    b"enum" => ReservedWord::Enum,
    b"trait" => ReservedWord::Trait,
    b"fn" => ReservedWord::Fn,
    b"const" => ReservedWord::Const,
    b"static" => ReservedWord::Static,
    b"impl" => ReservedWord::Impl,
    b"as" => ReservedWord::As,
    b"is" => ReservedWord::Is,
    b"let" => ReservedWord::Let,
    b"if" => ReservedWord::If,
    b"else" => ReservedWord::Else,
    b"match" => ReservedWord::Match,
    b"loop" => ReservedWord::Loop,
    b"while" => ReservedWord::While,
    b"for" => ReservedWord::For,
    b"break" => ReservedWord::Break,
    b"continue" => ReservedWord::Continue,
    b"return" => ReservedWord::Return,
    b"where" => ReservedWord::Where,
    b"Self" => ReservedWord::SelfUppercase,
    b"unsafe" => ReservedWord::Unsafe,
    b"_" => ReservedWord::Wildcard,
};

pub enum IdentifierOrReservedWord {
    Identifier(Identifier),
    ReservedWord(ReservedWord, Position),
}

impl From<Identifier> for IdentifierOrReservedWord {
    fn from(identifier: Identifier) -> Self {
        if identifier.get_kind() != IdentifierKind::Escaped
            && identifier.data.uses_small_optimization()
            && let Ok(reserved_word) = ReservedWord::try_from(identifier.as_ref())
        {
            Self::ReservedWord(reserved_word, identifier.position)
        } else {
            Self::Identifier(identifier)
        }
    }
}

/// Return type of `IdentifierOrReservedWord` parsing functions.
pub enum ParseResult {
    Ok(IdentifierOrReservedWord),
    Panic,
}

impl IdentifierOrReservedWord {
    /// Parse any kind of `Identifier` and check it for collision with reserved words. Assumes
    /// `stream` points to the first character of identifier sequence. If this returns
    /// `ParseResult::Panic` one `identifier::ParseError` is pushed onto error stack.
    pub fn parse<S, E>(stream: &mut S, errors: &mut E) -> Result<ParseResult, S::Error>
    where
        S: ByteStream + ?Sized,
        E: ErrorStack + ?Sized,
    {
        match Identifier::parse(stream, errors)? {
            identifier::ParseResult::Ok(identifier) => Ok(ParseResult::Ok(identifier.into())),
            identifier::ParseResult::Panic => Ok(ParseResult::Panic),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{super::tests::CheckIdentifierArguments, *};
    use parser_core::byte_stream::{core::Position, literal_byte_stream::LiteralByteStream};
    use strum::IntoEnumIterator;

    pub struct CheckReservedWordArguments {
        pub reserved_word: ReservedWord,
    }

    fn check_reserved_word(arguments: CheckReservedWordArguments) {
        let CheckReservedWordArguments { reserved_word } = arguments;
        let repr: &str = reserved_word.into();
        let mut stream = LiteralByteStream::new(repr.as_bytes().into());
        let mut errors = Vec::new();
        let position = stream.get_position().unwrap();
        match IdentifierOrReservedWord::parse(&mut stream, &mut errors).unwrap() {
            ParseResult::Ok(identifier_or_reserved_word) => match identifier_or_reserved_word {
                IdentifierOrReservedWord::ReservedWord(parsed_reserved_word, parsed_position) => {
                    assert_eq!(parsed_reserved_word, reserved_word);
                    assert_eq!(parsed_position, position);
                }
                IdentifierOrReservedWord::Identifier(identifier) => {
                    panic!(
                        "parsing result mismatch, expected `ReservedWord`, found: {:?}",
                        identifier
                    )
                }
            },
            ParseResult::Panic => panic!("unexpected parsing failure"),
        }
    }

    fn check_identifier(arguments: CheckIdentifierArguments) {
        let CheckIdentifierArguments {
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
        match IdentifierOrReservedWord::parse(&mut stream, &mut errors).unwrap() {
            ParseResult::Ok(identifier_or_reserved_word) => match identifier_or_reserved_word {
                IdentifierOrReservedWord::Identifier(parsed_identifier) => {
                    assert_eq!(
                        parsed_identifier,
                        Identifier::new_of_kind(identifier, position, identifier_kind)
                    );
                }
                IdentifierOrReservedWord::ReservedWord(reserved_word, _) => {
                    panic!(
                        "parsing result mismatch, expected `Identifier`, found: {:?}",
                        reserved_word
                    )
                }
            },
            ParseResult::Panic => panic!("unexpected parsing failure"),
        }
    }

    #[test]
    fn mapping_coherence() {
        for reserved_word in ReservedWord::iter() {
            let repr: &str = reserved_word.into();
            assert_eq!(reserved_word, *RESERVED_WORDS.get(repr.as_bytes()).unwrap());
        }
    }

    #[test]
    fn reserved_words() {
        for reserved_word in ReservedWord::iter() {
            check_reserved_word(CheckReservedWordArguments { reserved_word });
        }
    }

    #[test]
    fn escaped_reserved_words() {
        for reserved_word in ReservedWord::iter() {
            let repr: &str = reserved_word.into();
            check_identifier(CheckIdentifierArguments {
                identifier_kind: IdentifierKind::Escaped,
                identifier: repr.as_bytes(),
            });
        }
    }
}
