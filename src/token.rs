/// Represents a lexical token with a name/type and its raw value from the source code.
///
/// Used to represent the output of the [`Tokenizer`][crate::Tokenizer] struct.
///
/// # Examples
/// ```
/// # use crossandra::Token;
/// let num = Token { name: "int".into(), value: "23".into() };
/// let kw = Token::from(("keyword", "if"));
/// assert_eq!(num, Token::from(("int", "23")));
/// # assert_eq!(kw, Token { name: "keyword".into(), value: "if".into() });
/// # assert_eq!(format!("{num:?}"), "Token { name: \"int\", value: \"23\" }");
/// ```
#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    /// The type or category of the token (e.g., "int", "identifier", "operator").
    pub name: String,
    /// The actual text/value from the source code that this token represents.
    pub value: String,
}

impl<T: Into<String>> From<(T, T)> for Token {
    fn from(value: (T, T)) -> Self {
        Token {
            name: value.0.into(),
            value: value.1.into(),
        }
    }
}
