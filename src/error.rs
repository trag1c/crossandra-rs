#[derive(Debug)]
pub enum Error {
    BadToken(char),
    DuplicatePattern(String),
    EmptyLiteral,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BadToken(c) => write!(f, "invalid token {c:?}"),
            Self::DuplicatePattern(name) => write!(f, "duplicate pattern {name:?}"),
            Self::EmptyLiteral => write!(f, "literals cannot be empty"),
        }
    }
}
