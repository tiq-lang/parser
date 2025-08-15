//! TODO: write docs

use super::{InitialParseError, ParseError, ParseResult, Type};
use crate::byte_stream::extensions::TakeWhitespaces;
use parser_core::{
    byte_stream::core::{ByteStream, Position},
    error_stack::ErrorStack,
};
use std::{mem::replace, num::NonZero, ops::ControlFlow};

/// TODO: write docs
#[derive(Debug, PartialEq)]
pub struct Tuple {
    pub(crate) elements: Box<[Type]>,
    pub(crate) brace_positions: [Position; 2],
}

/// TODO: write docs
#[derive(Debug, PartialEq)]
pub struct Grouped {
    pub(crate) inner: Box<Type>,
    pub(crate) brace_positions: [Position; 2],
}

pub enum TupleOrGrouped {
    Tuple(Tuple),
    Grouped(Grouped),
}

impl From<Tuple> for TupleOrGrouped {
    fn from(value: Tuple) -> Self {
        Self::Tuple(value)
    }
}

impl From<Grouped> for TupleOrGrouped {
    fn from(value: Grouped) -> Self {
        Self::Grouped(value)
    }
}

enum ParserState {
    Initial,
    Tuple {
        opening_brace_position: Position,
        elements: Option<Vec<Type>>,
    },
    Exhausted,
}

pub struct TupleParser<'s, 'e, S, E>
where
    S: ByteStream + ?Sized,
    E: ErrorStack + ?Sized,
{
    stream: &'s mut S,
    errors: &'e mut E,
    state: ParserState,
}

enum ParseElementResult {
    Brace {
        preposition: Position,
    },
    TypeAndBrace {
        type_preposition: Position,
        r#type: Option<Type>,
    },
    TypeAndComma {
        type_preposition: Position,
        r#type: Option<Type>,
    },
    TypePanic,
    TypeError {
        preposition: Position,
        error: (InitialParseError, Position),
    },
    SeparatorError {
        preposition: Position,
        error_position: Position,
    },
}

fn parse_element<S, E>(stream: &mut S, errors: &mut E) -> Result<ParseElementResult, S::Error>
where
    S: ByteStream + ?Sized,
    E: ErrorStack + ?Sized,
{
    let mut preposition = stream.get_position()?;
    let type_preposition = preposition.clone();
    stream.take_whitespaces_mut()?;
    match stream.peek()? {
        Some(b')') => {
            // Safe because `stream.peek()` is `Some`
            unsafe { stream.advance_by(NonZero::new_unchecked(1))? }
            return Ok(ParseElementResult::Brace { preposition })
        }
        type_initial => {
            if let Err(error) = Type::check_initial(type_initial) {
                return Ok(ParseElementResult::TypeError {
                    preposition,
                    error: (error, stream.get_position()?),
                });
            }
        }
    }
    let r#type = match Type::parse(stream, errors)? {
        ParseResult::Ok(element) => Some(element),
        ParseResult::None => None,
        ParseResult::Panic => return Ok(ParseElementResult::TypePanic),
    };
    preposition = stream.get_position()?;
    stream.take_whitespaces_mut()?;
    match stream.peek()? {
        Some(b')') => {
            // Safe because `stream.peek()` is `Some`
            unsafe { stream.advance_by(NonZero::new_unchecked(1))? }
            Ok(ParseElementResult::TypeAndBrace {
                type_preposition,
                r#type,
            })
        }
        Some(b',') => {
            // Safe because `stream.peek()` is `Some`
            unsafe { stream.advance_by(NonZero::new_unchecked(1))? }
            Ok(ParseElementResult::TypeAndComma {
                type_preposition,
                r#type,
            })
        }
        _ => Ok(ParseElementResult::SeparatorError {
            preposition,
            error_position: stream.get_position()?,
        }),
    }
}

impl<'s, 'e, S, E> TupleParser<'s, 'e, S, E>
where
    S: ByteStream + ?Sized,
    E: ErrorStack + ?Sized,
{
    pub fn new(stream: &'s mut S, errors: &'e mut E) -> Self {
        Self {
            stream,
            errors,
            state: ParserState::Initial,
        }
    }

    /// TODO: write docs
    pub fn parse(mut self) -> Result<ParseResult<TupleOrGrouped>, S::Error> {
        loop {
            match self.run_iteration()? {
                ControlFlow::Continue(_) => (),
                ControlFlow::Break(result) => return Ok(result),
            }
        }
    }

    fn run_iteration(&mut self) -> Result<ControlFlow<ParseResult<TupleOrGrouped>>, S::Error> {
        match self.state {
            ParserState::Initial => self.run_initial_iteration(),
            ParserState::Tuple { .. } => self.run_tuple_iteration(),
            ParserState::Exhausted => unreachable!(),
        }
    }

    fn run_initial_iteration(
        &mut self,
    ) -> Result<ControlFlow<ParseResult<TupleOrGrouped>>, S::Error> {
        match &self.state {
            ParserState::Initial => (),
            _ => unreachable!(),
        }
        match parse_element(self.stream, self.errors)? {
            ParseElementResult::Brace { preposition } => {
                let tuple = Tuple {
                    elements: Box::new([]),
                    brace_positions: [
                        // Safe because `preposition` points to the byte directly after '('
                        unsafe { preposition.offset_unchecked(-1) },
                        // Safe because `self.stream` points to the byte directly after ')'
                        unsafe { self.stream.get_position()?.offset_unchecked(-1) },
                    ],
                };
                Ok(ControlFlow::Break(ParseResult::Ok(tuple.into())))
            }
            ParseElementResult::TypeAndBrace {
                type_preposition,
                r#type,
            } => {
                let result = if let Some(r#type) = r#type {
                    let grouped = Grouped {
                        inner: Box::new(r#type),
                        brace_positions: [
                            // Safe because `type_preposition` points to the byte directly after '('
                            unsafe { type_preposition.offset_unchecked(-1) },
                            // Safe because `self.stream` points to the byte directly after ')'
                            unsafe { self.stream.get_position()?.offset_unchecked(-1) },
                        ],
                    };
                    ParseResult::Ok(grouped.into())
                } else {
                    ParseResult::None
                };
                Ok(ControlFlow::Break(result))
            }
            ParseElementResult::TypeAndComma {
                type_preposition,
                r#type,
            } => {
                self.state = ParserState::Tuple {
                    // Safe because `type_preposition` points to the byte directly after '('
                    opening_brace_position: unsafe { type_preposition.offset_unchecked(-1) },
                    elements: r#type.map(|r#type| vec![r#type]),
                };
                Ok(ControlFlow::Continue(()))
            }
            ParseElementResult::TypePanic => Ok(ControlFlow::Break(ParseResult::Panic)),
            ParseElementResult::TypeError { preposition, error } => {
                self.errors
                    .push_erased_error(Box::new(ParseError::TupleOrGroupedInitial {
                        // Safe because `preposition` points to the byte directly after '('
                        opening_brace_position: unsafe { preposition.offset_unchecked(-1) },
                        error,
                    }));
                Ok(ControlFlow::Break(ParseResult::Panic))
            }
            ParseElementResult::SeparatorError {
                preposition,
                error_position,
            } => {
                self.errors
                    .push_erased_error(Box::new(ParseError::TupleOrGroupedUnexpectedSeparator {
                        preposition,
                        error_position,
                    }));
                Ok(ControlFlow::Break(ParseResult::Panic))
            }
        }
    }

    fn run_tuple_iteration(
        &mut self,
    ) -> Result<ControlFlow<ParseResult<TupleOrGrouped>>, S::Error> {
        match &self.state {
            ParserState::Tuple { .. } => (),
            _ => unreachable!(),
        }
        match parse_element(self.stream, self.errors)? {
            ParseElementResult::Brace { .. } => {
                let ParserState::Tuple {
                    opening_brace_position,
                    elements,
                } = replace(&mut self.state, ParserState::Exhausted)
                else {
                    unreachable!()
                };
                let result = if let Some(elements) = elements {
                    let tuple = Tuple {
                        elements: elements.into_boxed_slice(),
                        brace_positions: [
                            opening_brace_position,
                            // Safe because `self.stream` points to byte directly after ')'
                            unsafe { self.stream.get_position()?.offset_unchecked(-1) },
                        ],
                    }
                    .into();
                    ParseResult::Ok(tuple)
                } else {
                    ParseResult::None
                };
                Ok(ControlFlow::Break(result))
            }
            ParseElementResult::TypeAndBrace { r#type, .. } => {
                let ParserState::Tuple {
                    opening_brace_position,
                    elements,
                } = replace(&mut self.state, ParserState::Exhausted)
                else {
                    unreachable!()
                };
                let result = if let Some(mut elements) = elements
                    && let Some(r#type) = r#type
                {
                    elements.push(r#type);
                    let tuple = Tuple {
                        elements: elements.into_boxed_slice(),
                        brace_positions: [
                            opening_brace_position,
                            // Safe because `self.stream` points to the byte directly after '('
                            unsafe { self.stream.get_position()?.offset_unchecked(-1) },
                        ],
                    }
                    .into();
                    ParseResult::Ok(tuple)
                } else {
                    ParseResult::None
                };
                Ok(ControlFlow::Break(result))
            }
            ParseElementResult::TypeAndComma { r#type, .. } => {
                if let Some(r#type) = r#type
                    && let ParserState::Tuple {
                        elements: Some(elements),
                        ..
                    } = &mut self.state
                {
                    elements.push(r#type);
                }
                Ok(ControlFlow::Continue(()))
            }
            ParseElementResult::TypePanic => Ok(ControlFlow::Break(ParseResult::Panic)),
            ParseElementResult::TypeError { preposition, error } => {
                self.errors
                    .push_erased_error(Box::new(ParseError::TupleInitial { preposition, error }));
                Ok(ControlFlow::Break(ParseResult::Panic))
            }
            ParseElementResult::SeparatorError {
                preposition,
                error_position,
            } => {
                self.errors
                    .push_erased_error(Box::new(ParseError::TupleOrGroupedUnexpectedSeparator {
                        preposition,
                        error_position,
                    }));
                Ok(ControlFlow::Break(ParseResult::Panic))
            }
        }
    }
}
