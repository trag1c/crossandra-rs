#[derive(Debug)]
pub enum Error {
    BadToken(char, usize),
    DuplicatePattern(String),
    EmptyLiteral,
    InvalidRegex(Box<fancy_regex::Error>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BadToken(c, p) => write!(f, "invalid token {c:?} at position {p}"),
            Self::DuplicatePattern(name) => write!(f, "duplicate pattern {name:?}"),
            Self::EmptyLiteral => write!(f, "literals cannot be empty"),
            Self::InvalidRegex(err) => err.fmt(f),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::Error;

    #[test]
    fn error_display() {
        assert_eq!(
            Error::BadToken('x', 7).to_string(),
            "invalid token 'x' at position 7"
        );
        assert_eq!(
            Error::DuplicatePattern("string".into()).to_string(),
            "duplicate pattern \"string\""
        );
        assert_eq!(Error::EmptyLiteral.to_string(), "literals cannot be empty");
        assert_eq!(
            Error::InvalidRegex(Box::new(fancy_regex::Regex::new("+").unwrap_err())).to_string(),
            "Parsing error at position 0: Target of repeat operator is invalid"
        );
    }
}
