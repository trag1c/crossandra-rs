#[derive(Debug)]
pub enum Error {
    BadToken(char),
    DuplicatePattern(String),
    EmptyLiteral,
    InvalidRegex(Box<fancy_regex::Error>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BadToken(c) => write!(f, "invalid token {c:?}"),
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
    fn error_diplay() {
        assert_eq!(Error::BadToken('x').to_string(), "invalid token 'x'");
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
