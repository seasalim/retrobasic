use std::error;
use std::fmt;
use std::result;

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Error {
    SyntaxError(u32, String),
    RuntimeError(u32, String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Error::SyntaxError(l, ref e) => write!(f, "SYNTAX ERROR AT LINE {}: {}", l, e),
            &Error::RuntimeError(l, ref e) => write!(f, "RUNTIME ERROR AT LINE {}: {}", l, e),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match self {
            &Error::SyntaxError(_, _) => "Syntax Error",
            &Error::RuntimeError(_, _) => "Runtime Error",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        None
    }
}
