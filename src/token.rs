/// Represents a lexical token with a name/type and its raw value from the source code.
///
/// Used to represent the output of the [`Tokenizer`][crate::Tokenizer] struct.
///
/// # Examples
/// ```
/// # use crossandra::Token;
/// let num = Token { name: "int".into(), value: "23".into(), position: 3 };
/// let kw = Token::from(("keyword", "if", 0));
/// assert_eq!(num, Token::from(("int", "23", 3)));
/// # assert_eq!(kw, Token { name: "keyword".into(), value: "if".into(), position: 0 });
/// # assert_eq!(format!("{num:?}"), "Token { name: \"int\", value: \"23\", position: 3 }");
/// ```
#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    /// The type or category of the token (e.g., "int", "identifier", "operator").
    pub name: String,
    /// The actual text/value from the source code that this token represents.
    pub value: String,
    /// The position of the token in the source code.
    /// * For [`Tokenizer::tokenize`], this is the byte offset from the start of the entire source.
    /// * For [`Tokenizer::tokenize_lines`], this is the byte offset from the start of each line.
    ///
    /// For instance, tokenizing `"aa\naa"` for the token `a` will yield positions of `[0, 1, 3, 4]`
    /// and `[[0, 1], [0, 1]]`, respectively.
    pub position: usize,
}

impl<T: Into<String>> From<(T, T, usize)> for Token {
    fn from(value: (T, T, usize)) -> Self {
        Token {
            name: value.0.into(),
            value: value.1.into(),
            position: value.2,
        }
    }
}
