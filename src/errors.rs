use std::fmt;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub enum Error {
    Parsing(String),
    NotDefined(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parsing(msg) => write!(f, "Parsing error: {}", msg),
            Self::NotDefined(identifier) => write!(f, "Identifier \"{}\" not defined", identifier),
        }
    }
}

impl std::error::Error for Error {}
