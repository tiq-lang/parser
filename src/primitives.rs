use std::{ops::Range, path::Path, rc::Rc};

use crate::code_stream::{GetSpanInfoResult, SpanKind, StreamSource, SyntheticSpanSource};

mod identifier;

#[derive(Clone, Debug, PartialEq)]
pub enum Location {
    File(Rc<Path>, usize),
    Macro(Rc<Path>, Range<usize>),
    Derive(Rc<Path>, Range<usize>),
    Attribute(Rc<Path>, Range<usize>),
    Generated,
}

impl<'a> From<GetSpanInfoResult<'a>> for Location {
    fn from(value: GetSpanInfoResult<'a>) -> Self {
        match &value.span_info.kind {
            &SpanKind::Natural {
                ref file,
                position_in_file,
            } => Self::File(file.clone(), position_in_file + value.offset_in_span),
            &SpanKind::Synthetic {
                source,
                ref file,
                ref range,
            } => match source {
                SyntheticSpanSource::Macro => Self::Macro(file.clone(), range.clone()),
                SyntheticSpanSource::Derive => Self::Derive(file.clone(), range.clone()),
                SyntheticSpanSource::Attribute => Self::Attribute(file.clone(), range.clone()),
            },
        }
    }
}

#[derive(Debug)]
pub struct FileStreamSourceError;

impl TryFrom<&StreamSource> for Location {
    type Error = FileStreamSourceError;

    fn try_from(value: &StreamSource) -> Result<Self, Self::Error> {
        match value {
            &StreamSource::File(_) => Err(FileStreamSourceError),
            &StreamSource::Macro(ref file, ref range) => {
                Ok(Location::Macro(file.clone(), range.clone()))
            }
            &StreamSource::Derive(ref file, ref range) => {
                Ok(Location::Derive(file.clone(), range.clone()))
            }
            &StreamSource::Attribute(ref file, ref range) => {
                Ok(Location::Attribute(file.clone(), range.clone()))
            }
        }
    }
}
