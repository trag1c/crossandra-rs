//! Crossandra is a straightforward tokenization library designed for seamless text processing.
//!
//! # Examples
//! ## [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck)
//! ```rust
//! use crossandra::Tokenizer;
//!
//! # fn main() {
//! let bf_tok = Tokenizer::default()
//!     .with_literals(&[
//!         ("add", "+"),
//!         ("sub", "-"),
//!         ("left", "<"),
//!         ("right", ">"),
//!         ("read", ","),
//!         ("write", "."),
//!         ("begin_loop", "["),
//!         ("end_loop", "]"),
//!     ].into())
//!     .expect("all literals should be ≥1 characters long")
//!     .with_suppress_unknown(true);
//!
//! # assert!(bf_tok.tokenize("cat program: ,[.,]").is_ok());
//! match bf_tok.tokenize("cat program: ,[.,]") {
//!     Ok(tokens) => println!("{tokens:?}"),
//!     Err(e) => eprintln!("An error occurred: {e}"),
//! }
//! # }
//! ```
use std::collections::{HashMap, HashSet};

use fancy_regex::Regex;

pub mod common;

mod error;
use error::Error;

mod tree;
use tree::{generate_tree, Tree};

mod patterns;

const WHITESPACE: [char; 6] = [' ', '\x0c', '\t', '\x0b', '\r', '\n'];

/// Represents a lexical token with a name/type and its raw value from the source code.
///
/// Used to represent the output of the [`Tokenizer`] struct.
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

/// The Crossandra tokenizer, operating on literals and patterns.
///
/// ## Literals
/// Literals indicate values that have to be exactly matched by the tokenizer. They are represented
/// by a [`HashMap`], mapping the literal's name to its value. For instance, a literal map for
/// Brainfuck would be defined like this:
/// ```rust
/// # use std::collections::HashMap;
/// # use crossandra::Tokenizer;
/// let literals = HashMap::from([
///     ("add", "+"),
///     ("sub", "-"),
///     ("left", "<"),
///     ("right", ">"),
///     ("read", ","),
///     ("write", "."),
///     ("begin_loop", "["),
///     ("end_loop", "]"),
/// ]);
/// # assert!(Tokenizer::default().with_literals(&literals).is_ok());
/// ```
/// Literals take precedence over patterns.
///
/// ## Patterns
/// Patterns are regular expressions that match more complex token structures. They are represented
/// as pairs of strings (name, pattern) in a [`Vec`] to maintain a consistent matching order.
///
/// The order of patterns matters as the tokenizer will use the first matching pattern it finds.
/// Duplicate pattern names are not allowed and will result in an error. This crate also provides a
/// collection of commonly used patterns in the [`common`] module. For example, patterns covering
/// binary, octal, and hexadecimal literals could be defined like this:
/// ```rust
/// # use crossandra::Tokenizer;
/// let patterns = vec![
///     ("binary".into(), r"0[bB][01]+".into()),
///     ("octal".into(), r"0[Oo][0-7]+".into()),
///     ("hexadecimal".into(), r"(?i)0x[0-9a-f]+".into()),
/// ];
/// # assert!(Tokenizer::default().with_patterns(patterns).is_ok());
/// ```
///
/// ## Other options
///
/// ### `convert_crlf`
/// Whether to convert `\r\n` to `\n` before tokenization. Defaults to `true`.
///
/// ### `ignore_whitespace`
/// Whether to ignore the following whitespace characters:
///
/// | Code   | Character              |
/// |--------|------------------------|
/// | `0x9`  | Tab (`\t`)             |
/// | `0xa`  | Line feed (`\n`)       |
/// | `0xb`  | Vertical tab           |
/// | `0xc`  | Form feed              |
/// | `0xd`  | Carriage return (`\r`) |
/// | `0x20` | Space (` `)            |
///
/// Defaults to `false`.
///
/// ### `ignored_characters`
/// A set of characters to ignore during tokenization. Defaults to an empty [`Vec`].
///
/// ### `suppress_unknown`
/// Whether unknown tokens should halt tokenization or be silently ignored. Defaults to `false`.
///
/// ## Fast Mode
/// When all literals are of length 1 and there are no patterns, Crossandra uses a simpler
/// tokenization method.
///
/// For instance, tokenizing a 1MB random Brainfuck file with 10% of the file being comments is
/// ~300x faster with Fast Mode (32.5s vs 110ms on Apple M2).
///
/// Do note that this is a rather extreme case; for a 1KB file, the speedup is ~2.3x.
#[derive(Debug, Clone)]
pub struct Tokenizer<'a> {
    literals: HashMap<&'a str, &'a str>,
    patterns: Vec<(String, Regex)>,
    convert_crlf: bool,
    ignore_whitespace: bool,
    ignored_characters: Vec<char>,
    suppress_unknown: bool,
    tree: Tree,
}

impl PartialEq for Tokenizer<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.literals == other.literals
            && self.convert_crlf == other.convert_crlf
            && self.ignore_whitespace == other.ignore_whitespace
            && self.ignored_characters == other.ignored_characters
            && self.suppress_unknown == other.suppress_unknown
            && self.patterns.len() == other.patterns.len()
            && self
                .patterns
                .iter()
                .zip(&other.patterns)
                .all(|(a, b)| a.0 == b.0 && a.1.as_str() == b.1.as_str())
    }
}

impl Eq for Tokenizer<'_> {}

type InnerTokenizerFn<'a> = dyn Fn(&str) -> Result<Vec<Token>, Error> + 'a;

impl<'a> Tokenizer<'a> {
    /// Creates a new [`Tokenizer`] with the specified configuration.
    ///
    /// # Errors
    ///
    /// This function will return an error if:
    /// * any [literal](Tokenizer#literals) is empty,
    /// * there are duplicate [patterns](Tokenizer#patterns), or
    /// * any [pattern](Tokenizer#patterns) regex is invalid.
    pub fn new(
        literals: &HashMap<&'a str, &'a str>,
        patterns: Vec<(String, String)>,
        ignored_characters: Vec<char>,
        convert_crlf: bool,
        ignore_whitespace: bool,
        suppress_unknown: bool,
    ) -> Result<Self, Error> {
        validate_literals(literals)?;
        let literals = flip_hashmap(literals);
        Ok(Self {
            tree: generate_tree(&literals),
            literals,
            patterns: patterns::prepare(patterns)?,
            convert_crlf,
            ignored_characters,
            ignore_whitespace,
            suppress_unknown,
        })
    }

    fn can_use_fast_mode(&self) -> bool {
        self.patterns.is_empty() && self.literals.keys().all(|v| v.len() == 1)
    }

    fn prepare_source(&self, source: &str) -> String {
        if self.convert_crlf {
            source.replace("\r\n", "\n")
        } else {
            source.to_string()
        }
    }

    fn prepare_ignored(&self) -> HashSet<char> {
        if self.ignore_whitespace {
            self.ignored_characters
                .iter()
                .copied()
                .chain(WHITESPACE)
                .collect()
        } else {
            HashSet::from_iter(self.ignored_characters.clone())
        }
    }

    fn prepare_literal_map(&self) -> HashMap<char, &str> {
        self.literals
            .iter()
            .map(|(&k, &v)| (k.chars().next().expect("all literals should be 1-long"), v))
            .collect()
    }

    /// Tokenizes the given source code and returns a [`Vec`] of [`Token`]s.
    ///
    /// # Errors
    ///
    /// This function will return an error if an invalid token is found but not suppressed.
    pub fn tokenize(&self, source: &str) -> Result<Vec<Token>, Error> {
        let source = self.prepare_source(source);
        let ignored = self.prepare_ignored();
        if self.can_use_fast_mode() {
            self.tokenize_fast(&source, &ignored, &self.prepare_literal_map())
        } else {
            self.tokenize_core(&source, &ignored)
        }
    }

    /// Splits the given source code into lines and tokenizes each line separately.
    /// Returns a [`Vec`] of [`Vec`]s of [`Token`]s.
    ///
    /// # Errors
    ///
    /// This function will return an error if any line fails to tokenize.
    pub fn tokenize_lines(&self, source: &str) -> Result<Vec<Vec<Token>>, Error> {
        let ignored = self.prepare_ignored();
        let source = self.prepare_source(source);
        let func: Box<InnerTokenizerFn> = if self.can_use_fast_mode() {
            let literal_map = self.prepare_literal_map();
            Box::new(move |line| self.tokenize_fast(line, &ignored, &literal_map))
        } else {
            Box::new(|line| self.tokenize_core(line, &ignored))
        };
        source.split('\n').map(func).collect()
    }

    fn tokenize_core(
        &self,
        source: &str,
        ignored_characters: &HashSet<char>,
    ) -> Result<Vec<Token>, Error> {
        let mut tokens = Vec::new();
        let chunk_size = self.literals.keys().map(|x| x.len()).max().unwrap_or(1);

        let mut remaining_source = source;
        let mut chars = source.char_indices();

        'outer: while let Some((index, _)) = chars.find(|(_, c)| !ignored_characters.contains(c)) {
            remaining_source = &remaining_source[index..];

            let handling_result = self.handle(remaining_source, chunk_size);

            if let Ok((name, value, size)) = handling_result {
                tokens.push(Token { name, value });
                remaining_source = &remaining_source[size..];
                chars = remaining_source.char_indices();
                continue;
            }

            for (name, pattern) in &self.patterns {
                let Ok(Some(tok)) = pattern.find(remaining_source) else {
                    continue;
                };

                tokens.push(Token {
                    name: name.clone(),
                    value: tok.as_str().to_string(),
                });

                remaining_source = &remaining_source[tok.end()..];
                chars = remaining_source.char_indices();
                continue 'outer;
            }

            if !self.suppress_unknown {
                return Err(Error::BadToken(handling_result.unwrap_err()));
            }
        }
        Ok(tokens)
    }

    fn handle(
        &self,
        remaining_source: &str,
        chunk_size: usize,
    ) -> Result<(String, String, usize), char> {
        let mut break_path = None;
        let mut tree = &self.tree;

        let chunk_chars = remaining_source.char_indices().take(chunk_size);

        for (i, v) in chunk_chars {
            let Tree::Node(ref node) = tree else { continue };

            if let Some(t) = node.get(&None) {
                if let Tree::Leaf(token_name) = t {
                    break_path = Some((token_name, i));
                } else {
                    unreachable!("key None can never lead to a Node")
                }
            };

            match node.get(&Some(v)) {
                Some(Tree::Leaf(token_name)) => {
                    return Ok((
                        token_name.to_string(),
                        remaining_source[..=i].to_string(),
                        i + 1,
                    ));
                }
                Some(new_tree) => tree = new_tree,
                None => match node.get(&None) {
                    None => break,
                    Some(Tree::Leaf(token_name)) => {
                        return Ok((token_name.to_string(), remaining_source[..i].to_string(), i));
                    }
                    _ => unreachable!("key None can never lead to a Node"),
                },
            }
        }

        let joined_chunk = remaining_source
            .chars()
            .take(chunk_size)
            .collect::<String>();
        let joined_chunk_len = joined_chunk.len();

        if let Tree::Node(ref node) = tree {
            if let Some(t) = node.get(&None) {
                if let Tree::Leaf(token_name) = t {
                    break_path = Some((token_name, joined_chunk_len));
                } else {
                    unreachable!("key None can never lead to a Node")
                }
            };

            match node.get(&None) {
                None => {}
                Some(Tree::Leaf(token_name)) => {
                    return Ok((token_name.to_string(), joined_chunk, joined_chunk_len));
                }
                _ => unreachable!("key None can never lead to a Node"),
            }
        }

        if let Some((s, len)) = break_path {
            return Ok((
                s.to_string(),
                // FIXME: don't flip the hashmap every time, try caching it somewhere
                if let Some(&value) = flip_hashmap(&self.literals).get(s.as_str()) {
                    value.to_string()
                } else {
                    return Err(s.chars().next().expect("the token will never be unnamed"));
                },
                len,
            ));
        }

        if let Some(&name) = self.literals.get(joined_chunk.as_str()) {
            Ok((name.to_string(), joined_chunk, joined_chunk_len))
        } else {
            Err(joined_chunk
                .chars()
                .next()
                .expect("the chunk will never be empty"))
        }
    }

    fn tokenize_fast(
        &self,
        source: &str,
        ignored_characters: &HashSet<char>,
        literal_map: &HashMap<char, &str>,
    ) -> Result<Vec<Token>, Error> {
        let mut tokens: Vec<Token> = Vec::new();
        for char in source.chars() {
            if ignored_characters.contains(&char) {
                continue;
            }
            match literal_map.get(&char) {
                Some(name) => tokens.push(Token {
                    name: (*name).to_string(),
                    value: char.to_string(),
                }),
                None => {
                    if !self.suppress_unknown {
                        return Err(Error::BadToken(char));
                    }
                }
            }
        }
        Ok(tokens)
    }

    /// Sets the [literals](Tokenizer#literals) of this [`Tokenizer`] and returns itself.
    ///
    /// # Errors
    /// This function will return an error if any literal is empty.
    pub fn with_literals(mut self, literals: &HashMap<&'a str, &'a str>) -> Result<Self, Error> {
        self.set_literals(literals)?;
        Ok(self)
    }

    /// Sets the [patterns](Tokenizer#patterns) of this [`Tokenizer`] and returns itself.
    ///
    /// # Errors
    ///
    /// This function will return an error if:
    /// * there are duplicate patterns, or
    /// * any pattern regex is invalid.
    pub fn with_patterns(mut self, patterns: Vec<(String, String)>) -> Result<Self, Error> {
        self.set_patterns(patterns)?;
        Ok(self)
    }

    /// Sets the [ignored characters](Tokenizer#ignored_characters) of this [`Tokenizer`] and
    /// returns itself.
    #[must_use]
    pub fn with_ignored_characters(mut self, ignored_characters: Vec<char>) -> Self {
        self.ignored_characters = ignored_characters;
        self
    }

    /// Sets the [CRLF conversion](Tokenizer#convert_crlf) of this [`Tokenizer`] and returns itself.
    #[must_use]
    pub fn with_convert_crlf(mut self, convert_crlf: bool) -> Self {
        self.convert_crlf = convert_crlf;
        self
    }

    /// Sets the [`ignore_whitespace`](Tokenizer#ignore_whitespace) option of this [`Tokenizer`] and
    /// returns itself.
    #[must_use]
    pub fn with_ignore_whitespace(mut self, ignore_whitespace: bool) -> Self {
        self.ignore_whitespace = ignore_whitespace;
        self
    }

    /// Sets the [`suppress_unknown`](Tokenizer#suppress_unknown) option of this [`Tokenizer`] and
    /// returns itself.
    #[must_use]
    pub fn with_suppress_unknown(mut self, suppress_unknown: bool) -> Self {
        self.suppress_unknown = suppress_unknown;
        self
    }

    /// Sets the [literals](Tokenizer#literals) of this [`Tokenizer`].
    ///
    /// # Errors
    ///
    /// This function will return an error if any literal is empty.
    pub fn set_literals(&mut self, literals: &HashMap<&'a str, &'a str>) -> Result<(), Error> {
        validate_literals(literals)?;
        self.literals = flip_hashmap(literals);
        self.tree = generate_tree(&self.literals);
        Ok(())
    }

    /// Sets the [patterns](Tokenizer#patterns) of this [`Tokenizer`].
    ///
    /// # Errors
    ///
    /// This function will return an error if:
    /// * there are duplicate patterns, or
    /// * any pattern regex is invalid.
    pub fn set_patterns(&mut self, patterns: Vec<(String, String)>) -> Result<(), Error> {
        self.patterns = patterns::prepare(patterns)?;
        Ok(())
    }

    /// Sets the [ignored characters](Tokenizer#ignored_characters) of this [`Tokenizer`].
    pub fn set_ignored_characters(&mut self, ignored_characters: Vec<char>) {
        self.ignored_characters = ignored_characters;
    }

    /// Sets the [CRLF conversion](Tokenizer#convert_crlf) of this [`Tokenizer`].
    pub fn set_convert_crlf(&mut self, convert_crlf: bool) {
        self.convert_crlf = convert_crlf;
    }

    /// Sets the [`ignore_whitespace`](Tokenizer#ignore_whitespace) option of this [`Tokenizer`].
    pub fn set_ignore_whitespace(&mut self, ignore_whitespace: bool) {
        self.ignore_whitespace = ignore_whitespace;
    }

    /// Sets the [`suppress_unknown`](Tokenizer#suppress_unknown) option of this [`Tokenizer`].
    pub fn set_suppress_unknown(&mut self, suppress_unknown: bool) {
        self.suppress_unknown = suppress_unknown;
    }
}

impl<'a> Default for Tokenizer<'a> {
    fn default() -> Self {
        Self::new(&HashMap::new(), Vec::new(), Vec::new(), true, false, false)
            .expect("an empty tokenizer should be correct")
    }
}

fn flip_hashmap<'a>(hm: &HashMap<&'a str, &'a str>) -> HashMap<&'a str, &'a str> {
    hm.iter().map(|(&k, &v)| (v, k)).collect()
}

fn validate_literals<'a>(literals: &HashMap<&'a str, &'a str>) -> Result<(), Error> {
    literals
        .values()
        .all(|literal| !literal.is_empty())
        .then_some(())
        .ok_or(Error::EmptyLiteral)
}

#[cfg(test)]
mod tests {
    use crate::{
        common,
        error::Error,
        flip_hashmap,
        tree::{generate_tree, Tree},
        validate_literals, Token, Tokenizer,
    };
    use std::collections::{HashMap, HashSet};

    #[test]
    fn literal_validation_err() {
        assert!(matches!(
            validate_literals(&HashMap::from([("x", "x"), ("y", "")])),
            Err(Error::EmptyLiteral)
        ));
    }

    #[test]
    fn literal_validation_ok() {
        assert!(validate_literals(&HashMap::from([("x", "x"), ("", "y")])).is_ok());
    }

    #[test]
    fn flip_hashmap_ok() {
        assert_eq!(flip_hashmap(&HashMap::new()), HashMap::new());
        assert_eq!(
            flip_hashmap(&HashMap::from([("a", "b"), ("c", "d")])),
            HashMap::from([("b", "a"), ("d", "c")])
        );
    }

    #[test]
    fn fast_mode() {
        let tests = [
            (HashMap::new(), Vec::new(), true),
            (HashMap::new(), vec![(String::new(), String::new())], false),
            (HashMap::from([("", "a")]), Vec::new(), true),
            (HashMap::from([("", "a"), ("", "b")]), Vec::new(), true),
            (
                HashMap::from([("", "a"), ("", "b"), ("", "c!")]),
                Vec::new(),
                false,
            ),
        ];
        for (literals, patterns, expected) in tests {
            assert_eq!(
                Tokenizer::default()
                    .with_literals(&literals)
                    .unwrap()
                    .with_patterns(patterns)
                    .unwrap()
                    .can_use_fast_mode(),
                expected
            );
        }
    }

    #[test]
    fn source_preparation() {
        let tests = [(true, "foo\nbar"), (false, "foo\r\nbar")];
        for (convert_crlf, expected) in tests {
            assert_eq!(
                Tokenizer::default()
                    .with_convert_crlf(convert_crlf)
                    .prepare_source("foo\r\nbar"),
                expected
            );
        }
    }

    #[test]
    fn ignored_preparation() {
        let tests = [
            (vec![], false, HashSet::new()),
            (vec!['a', 'b', 'c'], false, "abc".chars().collect()),
            (
                vec!['a', 'b', 'c'],
                true,
                "abc \x0c\t\x0b\r\n".chars().collect(),
            ),
        ];
        for (ignored_characters, ignore_whitespace, expected) in tests {
            assert_eq!(
                Tokenizer::default()
                    .with_ignore_whitespace(ignore_whitespace)
                    .with_ignored_characters(ignored_characters)
                    .prepare_ignored(),
                expected
            );
        }
    }

    #[test]
    fn literal_map_preparation() {
        assert_eq!(
            Tokenizer::default()
                .with_literals(&HashMap::from([("x", "a"), ("y", "b")]))
                .unwrap()
                .prepare_literal_map(),
            HashMap::from([('a', "x"), ('b', "y")])
        );
    }

    #[test]
    fn comparison() {
        let def = Tokenizer::default();
        assert_eq!(def, Tokenizer::default());
        assert_eq!(def, Tokenizer::default().with_ignored_characters([].into()));
        assert_ne!(
            def,
            Tokenizer::default().with_ignored_characters(['x'].into())
        );
        assert_ne!(def, Tokenizer::default().with_ignore_whitespace(true));
        assert_ne!(def, Tokenizer::default().with_suppress_unknown(true));
        assert_ne!(def, Tokenizer::default().with_convert_crlf(false));
        assert_ne!(
            def,
            Tokenizer::default()
                .with_literals(&[("1", "2")].into())
                .unwrap()
        );
        assert_eq!(
            def.clone().with_literals(&[("1", "2")].into()).unwrap(),
            Tokenizer::default()
                .with_literals(&[("1", "2")].into())
                .unwrap()
        );
        assert_ne!(
            def,
            Tokenizer::default()
                .with_patterns([("1".into(), "2".into())].into())
                .unwrap()
        );
    }

    #[test]
    fn builder_equivalence() {
        let literals = HashMap::from([("a", "b")]);
        let patterns = vec![(String::from("a"), String::from("b"))];
        let ignored_chars = vec!['x'];

        let mut tok1 = Tokenizer::default();
        tok1.set_convert_crlf(false);
        tok1.set_ignore_whitespace(true);
        tok1.set_suppress_unknown(true);
        tok1.set_ignored_characters(ignored_chars.clone());
        tok1.set_literals(&literals).unwrap();
        tok1.set_patterns(patterns.clone()).unwrap();

        let tok2 = Tokenizer::default()
            .with_convert_crlf(false)
            .with_ignore_whitespace(true)
            .with_suppress_unknown(true)
            .with_ignored_characters(ignored_chars.clone())
            .with_literals(&literals)
            .unwrap()
            .with_patterns(patterns.clone())
            .unwrap();

        let tok3 = Tokenizer::new(&literals, patterns, ignored_chars, false, true, true).unwrap();

        assert_eq!(tok1, tok2);
        assert_eq!(tok1, tok3);
        assert_eq!(tok2, tok3);
    }

    #[test]
    fn builder_processing_literals() {
        let mut tok = Tokenizer::default();
        assert_eq!(tok.tree, Tree::Node(HashMap::new()));

        let literals = HashMap::from([("a", "b")]);
        assert!(tok.set_literals(&literals).is_ok());

        let flipped_literals = HashMap::from([("b", "a")]);
        let expected_tree = generate_tree(&flipped_literals);
        assert_eq!(tok.literals, flipped_literals);
        assert_eq!(tok.tree, expected_tree);

        assert!(tok.set_literals(&HashMap::from([("a", "")])).is_err());
    }

    #[test]
    fn builder_processing_patterns() {
        let mut tok = Tokenizer::default();
        assert!(tok.patterns.is_empty());

        let pattern = ("a".into(), r"\d+".into());
        assert!(tok.set_patterns(vec![pattern.clone()]).is_ok());
        assert_eq!(tok.patterns.first().unwrap().1.as_str(), r"^(\d+)");

        assert!(tok.set_patterns(vec![pattern.clone(), pattern]).is_ok());
        assert!(tok.set_patterns(vec![("a".into(), "+".into())]).is_err());
    }

    #[test]
    fn empty_tokenizer() {
        let tok = Tokenizer::default();

        assert!(matches!(tok.tokenize(""), Ok(v) if v.is_empty()));

        match tok.tokenize("source") {
            Err(Error::BadToken(c)) => assert_eq!(c, 's'),
            _ => panic!("tokenization didn't fail with BadToken"),
        }
    }

    #[test]
    fn brainfuck_fast_tokenizer() {
        let tok = Tokenizer::default()
            .with_literals(&HashMap::from_iter([
                ("add", "+"),
                ("sub", "-"),
                ("left", "<"),
                ("right", ">"),
                ("read", ","),
                ("write", "."),
                ("begin_loop", "["),
                ("end_loop", "]"),
            ]))
            .unwrap();

        if let Ok(tokens) = tok.tokenize(",[.,]") {
            assert!(tokens
                .iter()
                .map(|t| t.name.clone())
                .eq("read begin_loop write read end_loop".split_whitespace()));
        } else {
            panic!();
        };
    }

    fn make_output(tokens: Vec<(&str, &str)>) -> Vec<Token> {
        tokens.into_iter().map(Token::from).collect()
    }

    #[test]
    fn arithmetic_tokenizer() {
        let tests = [
            (
                "2 * 2 + 3 - 7",
                make_output(vec![
                    ("int", "2"),
                    ("mul", "*"),
                    ("int", "2"),
                    ("add", "+"),
                    ("int", "3"),
                    ("sub", "-"),
                    ("int", "7"),
                ]),
            ),
            (
                "2**3",
                make_output(vec![("int", "2"), ("pow", "**"), ("int", "3")]),
            ),
            ("-5", make_output(vec![("sub", "-"), ("int", "5")])),
            (
                "100 + -5",
                make_output(vec![
                    ("int", "100"),
                    ("add", "+"),
                    ("sub", "-"),
                    ("int", "5"),
                ]),
            ),
            (
                "4 - 2 ** 5 / 2",
                make_output(vec![
                    ("int", "4"),
                    ("sub", "-"),
                    ("int", "2"),
                    ("pow", "**"),
                    ("int", "5"),
                    ("div", "/"),
                    ("int", "2"),
                ]),
            ),
            (
                "10 % 3",
                make_output(vec![("int", "10"), ("mod", "%"), ("int", "3")]),
            ),
        ];

        let tok = Tokenizer::default()
            .with_literals(
                &[
                    ("add", "+"),
                    ("sub", "-"),
                    ("mul", "*"),
                    ("div", "/"),
                    ("pow", "**"),
                    ("mod", "%"),
                ]
                .into(),
            )
            .unwrap()
            .with_patterns(vec![common::INT.clone()])
            .unwrap()
            .with_suppress_unknown(true);

        for (input, output) in tests {
            assert_eq!(tok.tokenize(input).expect("tokenization failed"), output);
        }
    }

    #[test]
    fn line_tokenization() {
        let tok = Tokenizer::default()
            .with_ignore_whitespace(true)
            .with_patterns(vec![common::WORD.clone()])
            .unwrap();
        let Ok(lines) = tok.tokenize_lines("a b\nc\rde") else {
            panic!("tokenization failed");
        };
        assert_eq!(
            lines,
            vec![
                make_output(vec![("word", "a"), ("word", "b")]),
                make_output(vec![("word", "c"), ("word", "de")]),
            ]
        );
    }

    #[test]
    fn breakpoint_tokenization() {
        let (x, y, z) = (("x", "abc"), ("y", "a"), ("z", "b"));
        let tok = Tokenizer::default()
            .with_literals(&[x, y, z].into())
            .unwrap();
        let Ok(tokens) = tok.tokenize("ababaababc") else {
            panic!("tokenization failed");
        };
        assert_eq!(tokens, make_output(vec![y, z, y, z, y, y, z, x]));
    }

    #[test]
    fn fast_tokenization_with_ignoreset() {
        let literals = [("foo", "x"), ("bar", "y")];
        let tok = Tokenizer::default()
            .with_literals(&literals.into())
            .unwrap()
            .with_ignored_characters(['z'].into());
        let Ok(tokens) = tok.tokenize("xzy") else {
            panic!("tokenization failed");
        };
        assert_eq!(tokens, make_output(literals.into()));
    }

    #[test]
    fn core_tokenization_with_ignoreset() {
        let (foo, bar) = (("foo", "xz"), ("bar", "yz"));
        let tok = Tokenizer::default()
            .with_literals(&[foo, bar].into())
            .unwrap()
            .with_ignored_characters(['z'].into());
        let Ok(tokens) = tok.tokenize("zxzyzxzyzzzyzzxzyzzzxzz") else {
            panic!("tokenization failed");
        };
        assert_eq!(
            tokens,
            make_output(vec![foo, bar, foo, bar, bar, foo, bar, foo])
        );
    }

    #[test]
    fn whitespace_tokenization() {
        let (cr, ln, space) = (("cr", "\r"), ("ln", "\n"), ("space", " "));
        let mut tok = Tokenizer::default()
            .with_literals(&[cr, ln, space].into())
            .unwrap();
        let source = " \r\n \r \n ";

        let tests = [
            (
                true,
                vec![space, ln, space, cr, space, ln, space],
                vec![
                    vec![Token::from(space)],
                    make_output(vec![space, cr, space]),
                    vec![Token::from(space)],
                ],
            ),
            (
                false,
                vec![space, cr, ln, space, cr, space, ln, space],
                vec![
                    make_output(vec![space, cr]),
                    make_output(vec![space, cr, space]),
                    vec![Token::from(space)],
                ],
            ),
        ];
        for (convert_crlf, single_line_output, multi_line_output) in tests {
            tok.set_convert_crlf(convert_crlf);
            let Ok(tokens) = tok.tokenize(source) else {
                panic!("tokenization failed");
            };
            assert_eq!(tokens, make_output(single_line_output));

            let Ok(lines) = tok.tokenize_lines(source) else {
                panic!("tokenization failed");
            };
            assert_eq!(lines, multi_line_output);
        }
    }

    #[test]
    fn bad_tokenization_fast() {
        let tok = Tokenizer::default();
        let Error::BadToken(bad_token) = tok.tokenize("x").unwrap_err() else {
            panic!("tokenization didn't fail with BadToken");
        };
        assert_eq!(bad_token, 'x');
    }

    #[test]
    fn bad_tokenization_core() {
        let tok = Tokenizer::default()
            .with_literals(&[("xy", "xy")].into())
            .unwrap();
        let Error::BadToken(bad_token) = tok.tokenize("xyz").unwrap_err() else {
            panic!("tokenization didn't fail with BadToken");
        };
        assert_eq!(bad_token, 'z');
    }
}
