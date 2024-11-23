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
//!     .expect("all literals should be ≥1 characters long");
//!
//! for token in bf_tok.tokenize("cat program: ,[.,]").filter_map(Result::ok) {
//!    println!("{:?}", token);
//! }
//! # }
//! ```
use std::collections::{HashMap, HashSet};

use fancy_regex::Regex;

pub mod common;

mod error;
use error::Error;

mod stream;

mod token;
pub use token::Token;

mod tree;
use tree::{generate_tree, Tree};

mod patterns;

const WHITESPACE: [char; 6] = [' ', '\x0c', '\t', '\x0b', '\r', '\n'];

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
    ignore_whitespace: bool,
    ignored_characters: HashSet<char>,
    tree: Tree,
}

impl PartialEq for Tokenizer<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.literals == other.literals
            && self.ignore_whitespace == other.ignore_whitespace
            && self.ignored_characters == other.ignored_characters
            && self.patterns.len() == other.patterns.len()
            && self
                .patterns
                .iter()
                .zip(&other.patterns)
                .all(|(a, b)| a.0 == b.0 && a.1.as_str() == b.1.as_str())
    }
}

impl Eq for Tokenizer<'_> {}

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
        ignored_characters: HashSet<char>,
        ignore_whitespace: bool,
    ) -> Result<Self, Error> {
        validate_literals(literals)?;
        let literals = stream::flip_hashmap(literals);
        Ok(Self {
            tree: generate_tree(&literals),
            literals,
            patterns: patterns::prepare(patterns)?,
            ignored_characters,
            ignore_whitespace,
        })
    }

    fn can_use_fast_mode(&self) -> bool {
        self.patterns.is_empty() && self.literals.keys().all(|v| v.len() == 1)
    }

    fn prepare_ignored(&self) -> HashSet<char> {
        let ignored = self.ignored_characters.iter().copied();
        if self.ignore_whitespace {
            ignored.chain(WHITESPACE).collect()
        } else {
            ignored.collect()
        }
    }

    /// Tokenizes the given source code and returns an [`Iterator`] of [`Token`]s.
    #[must_use]
    pub fn tokenize(
        &'a self,
        source: &'a str,
    ) -> Box<dyn Iterator<Item = Result<Token, Error>> + 'a> {
        let ignored = self.prepare_ignored();
        if self.can_use_fast_mode() {
            let chars = source.chars();
            if ignored.is_empty() {
                Box::new(stream::Fast::new(self, chars))
            } else {
                Box::new(stream::Fast::new(
                    self,
                    chars.filter(move |c| !ignored.contains(c)),
                ))
            }
        } else {
            Box::new(stream::Core::new(self, source, ignored))
        }
    }

    /// Splits the given source code into lines and tokenizes each line separately.
    /// Returns an [`Iterator`] of [`Vec`]s of [`Token`]s.
    ///
    /// # Errors
    ///
    /// This function will return an error if any line fails to tokenize.
    #[must_use]
    pub fn tokenize_lines(
        &'a self,
        source: &'a str,
    ) -> Box<dyn Iterator<Item = Result<Vec<Token>, Error>> + 'a> {
        Box::new(source.split('\n').map(|line| self.tokenize(line).collect()))
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
    pub fn with_ignored_characters(mut self, ignored_characters: HashSet<char>) -> Self {
        self.ignored_characters = ignored_characters;
        self
    }

    /// Sets the [`ignore_whitespace`](Tokenizer#ignore_whitespace) option of this [`Tokenizer`] and
    /// returns itself.
    #[must_use]
    pub fn with_ignore_whitespace(mut self, ignore_whitespace: bool) -> Self {
        self.ignore_whitespace = ignore_whitespace;
        self
    }

    /// Sets the [literals](Tokenizer#literals) of this [`Tokenizer`].
    ///
    /// # Errors
    ///
    /// This function will return an error if any literal is empty.
    pub fn set_literals(&mut self, literals: &HashMap<&'a str, &'a str>) -> Result<(), Error> {
        validate_literals(literals)?;
        self.literals = stream::flip_hashmap(literals);
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
    pub fn set_ignored_characters(&mut self, ignored_characters: HashSet<char>) {
        self.ignored_characters = ignored_characters;
    }

    /// Sets the [`ignore_whitespace`](Tokenizer#ignore_whitespace) option of this [`Tokenizer`].
    pub fn set_ignore_whitespace(&mut self, ignore_whitespace: bool) {
        self.ignore_whitespace = ignore_whitespace;
    }
}

impl Default for Tokenizer<'_> {
    fn default() -> Self {
        Self::new(&HashMap::new(), Vec::new(), HashSet::new(), false)
            .expect("an empty tokenizer should be correct")
    }
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
    use super::*;

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
    fn ignored_preparation() {
        let tests = [
            ([].into(), false, HashSet::new()),
            (['a', 'b', 'c'].into(), false, "abc".chars().collect()),
            (
                ['a', 'b', 'c'].into(),
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
    fn comparison() {
        let def = Tokenizer::default();
        assert_eq!(def, Tokenizer::default());
        assert_eq!(def, Tokenizer::default().with_ignored_characters([].into()));
        assert_ne!(
            def,
            Tokenizer::default().with_ignored_characters(['x'].into())
        );
        assert_ne!(def, Tokenizer::default().with_ignore_whitespace(true));
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
        let ignored_chars: HashSet<_> = ['x'].into();

        let mut tok1 = Tokenizer::default();
        tok1.set_ignore_whitespace(true);
        tok1.set_ignored_characters(ignored_chars.clone());
        tok1.set_literals(&literals).unwrap();
        tok1.set_patterns(patterns.clone()).unwrap();

        let tok2 = Tokenizer::default()
            .with_ignore_whitespace(true)
            .with_ignored_characters(ignored_chars.clone())
            .with_literals(&literals)
            .unwrap()
            .with_patterns(patterns.clone())
            .unwrap();

        let tok3 = Tokenizer::new(&literals, patterns, ignored_chars, true).unwrap();

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

        assert!(tok.tokenize("").next().is_none());

        match tok.tokenize("source").next() {
            Some(Err(Error::BadToken(c))) => assert_eq!(c, 's'),
            _ => panic!("tokenization didn't fail with BadToken"),
        };
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

        if let Ok(tokens) = tok.tokenize(",[.,]").collect::<Result<Vec<_>, _>>() {
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
            .unwrap();

        for (input, output) in tests {
            assert_eq!(
                tok.tokenize(input)
                    .filter_map(Result::ok)
                    .collect::<Vec<_>>(),
                output
            );
        }
    }

    #[test]
    fn line_tokenization() {
        let tok = Tokenizer::default()
            .with_ignore_whitespace(true)
            .with_patterns(vec![common::WORD.clone()])
            .unwrap();
        let Ok(lines) = tok
            .tokenize_lines("a b\nc\rde")
            .collect::<Result<Vec<Vec<Token>>, _>>()
        else {
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
    fn line_tokenization_fast() {
        let (a, b) = (("a", "a"), ("b", "b"));
        let tok = Tokenizer::default()
            .with_ignore_whitespace(true)
            .with_literals(&[a, b].into())
            .unwrap();
        let Ok(lines) = tok
            .tokenize_lines("a b\nb\ra")
            .collect::<Result<Vec<Vec<Token>>, _>>()
        else {
            panic!("tokenization failed");
        };
        assert_eq!(
            lines,
            vec![make_output(vec![a, b]), make_output(vec![b, a])]
        );
    }

    #[test]
    fn breakpoint_tokenization() {
        let (x, y, z) = (("x", "abc"), ("y", "a"), ("z", "b"));
        let tok = Tokenizer::default()
            .with_literals(&[x, y, z].into())
            .unwrap();
        let Ok(tokens) = tok.tokenize("ababaababc").collect::<Result<Vec<_>, _>>() else {
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
        let Ok(tokens) = tok.tokenize("xzy").collect::<Result<Vec<_>, _>>() else {
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
        let Ok(tokens) = tok
            .tokenize("zxzyzxzyzzzyzzxzyzzzxzz")
            .collect::<Result<Vec<_>, _>>()
        else {
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
        let tok = Tokenizer::default()
            .with_literals(&[cr, ln, space].into())
            .unwrap();
        let source = " \r\n \r \n ";

        let Ok(tokens) = tok.tokenize(source).collect::<Result<Vec<_>, _>>() else {
            panic!("tokenization failed");
        };
        assert_eq!(
            tokens,
            make_output(vec![space, cr, ln, space, cr, space, ln, space])
        );

        let Ok(lines) = tok
            .tokenize_lines(source)
            .collect::<Result<Vec<Vec<Token>>, _>>()
        else {
            panic!("tokenization failed");
        };
        assert_eq!(
            lines,
            vec![
                make_output(vec![space, cr]),
                make_output(vec![space, cr, space]),
                vec![Token::from(space)],
            ]
        );
    }

    #[test]
    fn bad_tokenization_fast() {
        let tok = Tokenizer::default();
        let Some(Err(Error::BadToken(bad_token))) = tok.tokenize("x").last() else {
            panic!("tokenization didn't fail with BadToken");
        };
        assert_eq!(bad_token, 'x');
    }

    #[test]
    fn bad_tokenization_core() {
        let tok = Tokenizer::default()
            .with_literals(&[("xy", "xy")].into())
            .unwrap();
        let Some(Err(Error::BadToken(bad_token))) = tok.tokenize("xyz").find(Result::is_err) else {
            panic!("tokenization didn't fail with BadToken");
        };
        assert_eq!(bad_token, 'z');
    }
}
