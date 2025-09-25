//! Crossandra is a straightforward tokenization library designed for seamless text processing.
//!
//! # Examples
//! ## [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck)
//! ```rust
//! use crossandra::Tokenizer;
//! use rustc_hash::FxHashMap;
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
//!     ])
//!     .expect("all literals should be ≥1 characters long");
//!
//! for token in bf_tok.tokenize("cat program: ,[.,]").filter_map(Result::ok) {
//!    println!("{:?}", token);
//! }
//! # }
//! ```
pub use rustc_hash::{FxHashMap, FxHashSet};

use fancy_regex::Regex;
use rayon::prelude::*;

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
/// by a slice of (name, value) pairs. For example, a literal map for Brainfuck would be defined
/// like this:
/// ```rust
/// # use crossandra::Tokenizer;
/// let literals = [
///     ("add", "+"),
///     ("sub", "-"),
///     ("left", "<"),
///     ("right", ">"),
///     ("read", ","),
///     ("write", "."),
///     ("begin_loop", "["),
///     ("end_loop", "]"),
/// ];
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
    literals: FxHashMap<&'a str, &'a str>,
    patterns: Vec<(String, Regex)>,
    ignore_whitespace: bool,
    ignored_characters: FxHashSet<char>,
    tree: Tree<'a>,
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
        literals: &[(&'a str, &'a str)],
        patterns: Vec<(String, String)>,
        ignored_characters: FxHashSet<char>,
        ignore_whitespace: bool,
    ) -> Result<Self, Error> {
        validate_literals(literals)?;
        let literals = stream::build_hashmap(literals);
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

    fn prepare_ignored(&self) -> FxHashSet<char> {
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
    ) -> Box<dyn Iterator<Item = Result<Token<'a>, Error>> + 'a> {
        let ignored = self.prepare_ignored();
        if self.can_use_fast_mode() {
            Box::new(stream::Fast::new(self, source, ignored))
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
    ) -> impl ParallelIterator<Item = Result<Vec<Token<'a>>, Error>> + 'a {
        source
            .par_split('\n')
            .map(|line| self.tokenize(line).collect())
    }

    /// Sets the [literals](Tokenizer#literals) of this [`Tokenizer`] and returns itself.
    ///
    /// # Errors
    /// This function will return an error if any literal is empty.
    pub fn with_literals(mut self, literals: &[(&'a str, &'a str)]) -> Result<Self, Error> {
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
    pub fn with_ignored_characters(mut self, ignored_characters: FxHashSet<char>) -> Self {
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
    pub fn set_literals(&mut self, literals: &[(&'a str, &'a str)]) -> Result<(), Error> {
        validate_literals(literals)?;
        self.literals = stream::build_hashmap(literals);
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
    pub fn set_ignored_characters(&mut self, ignored_characters: FxHashSet<char>) {
        self.ignored_characters = ignored_characters;
    }

    /// Sets the [`ignore_whitespace`](Tokenizer#ignore_whitespace) option of this [`Tokenizer`].
    pub fn set_ignore_whitespace(&mut self, ignore_whitespace: bool) {
        self.ignore_whitespace = ignore_whitespace;
    }
}

impl Default for Tokenizer<'_> {
    fn default() -> Self {
        Self::new(&[], Vec::new(), FxHashSet::default(), false)
            .expect("an empty tokenizer should be correct")
    }
}

fn validate_literals<'a>(literals: &[(&'a str, &'a str)]) -> Result<(), Error> {
    literals
        .iter()
        .all(|(_name, token)| !token.is_empty())
        .then_some(())
        .ok_or(Error::EmptyLiteral)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_validation_err() {
        assert!(matches!(
            validate_literals(&[("x", "x"), ("y", "")]),
            Err(Error::EmptyLiteral)
        ));
    }

    #[test]
    fn literal_validation_ok() {
        assert!(validate_literals(&[("x", "x"), ("", "y")]).is_ok());
    }

    #[test]
    fn fast_mode() {
        let tests: [(&[_], Vec<_>, bool); 5] = [
            (&[], Vec::new(), true),
            (&[], vec![(String::new(), String::new())], false),
            (&[("", "a")], Vec::new(), true),
            (&[], Vec::new(), true),
            (&[("", "a"), ("", "b"), ("", "c!")], Vec::new(), false),
        ];
        for (literals, patterns, expected) in tests {
            assert_eq!(
                Tokenizer::default()
                    .with_literals(literals)
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
            (FxHashSet::default(), false, FxHashSet::default()),
            (
                FxHashSet::from_iter(['a', 'b', 'c']),
                false,
                "abc".chars().collect(),
            ),
            (
                FxHashSet::from_iter(['a', 'b', 'c']),
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
        assert_eq!(
            def,
            Tokenizer::default().with_ignored_characters(FxHashSet::default())
        );
        assert_ne!(
            def,
            Tokenizer::default().with_ignored_characters(FxHashSet::from_iter(['x']))
        );
        assert_ne!(def, Tokenizer::default().with_ignore_whitespace(true));
        assert_ne!(
            def,
            Tokenizer::default().with_literals(&[("1", "2")]).unwrap()
        );
        assert_eq!(
            def.clone().with_literals(&[("1", "2")]).unwrap(),
            Tokenizer::default().with_literals(&[("1", "2")]).unwrap()
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
        let literals = [("a", "b")];
        let patterns = vec![(String::from("a"), String::from("b"))];
        let ignored_chars: FxHashSet<_> = FxHashSet::from_iter(['x']);

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
        assert_eq!(tok.tree, Tree::Node(FxHashMap::default()));

        assert!(tok.set_literals(&[("a", "b")]).is_ok());

        let flipped_literals = FxHashMap::from_iter([("b", "a")]);
        let expected_tree = generate_tree(&flipped_literals);
        assert_eq!(tok.literals, flipped_literals);
        assert_eq!(tok.tree, expected_tree);

        assert!(tok.set_literals(&[("a", "")]).is_err());
    }

    #[test]
    fn builder_processing_patterns() {
        let mut tok = Tokenizer::default();
        assert!(tok.patterns.is_empty());

        let pattern = ("a".into(), r"\d+".into());
        assert!(tok.set_patterns(vec![pattern.clone()]).is_ok());
        assert_eq!(tok.patterns.first().unwrap().1.as_str(), r"^(?:\d+)");

        assert!(tok.set_patterns(vec![pattern.clone(), pattern]).is_ok());
        assert!(tok.set_patterns(vec![("a".into(), "+".into())]).is_err());
    }

    #[test]
    fn empty_tokenizer() {
        let tok = Tokenizer::default();

        assert!(tok.tokenize("").next().is_none());

        match tok.tokenize("source").next() {
            Some(Err(Error::BadToken(c, p))) => assert_eq!((c, p), ('s', 0)),
            _ => panic!("tokenization didn't fail with BadToken"),
        };
    }

    #[test]
    fn brainfuck_fast_tokenizer() {
        let tok = Tokenizer::default()
            .with_literals(&[
                ("add", "+"),
                ("sub", "-"),
                ("left", "<"),
                ("right", ">"),
                ("read", ","),
                ("write", "."),
                ("begin_loop", "["),
                ("end_loop", "]"),
            ])
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

    fn make_output<'a>(tokens: Vec<((&'a str, &'a str), usize)>) -> Vec<Token<'a>> {
        tokens
            .into_iter()
            .map(|((n, v), p)| Token::from((n, v, p)))
            .collect()
    }

    #[test]
    fn arithmetic_tokenizer() {
        let tests = [
            (
                "2 * 2 + 3 - 7",
                make_output(vec![
                    (("int", "2"), 0),
                    (("mul", "*"), 2),
                    (("int", "2"), 4),
                    (("add", "+"), 6),
                    (("int", "3"), 8),
                    (("sub", "-"), 10),
                    (("int", "7"), 12),
                ]),
            ),
            (
                "2**3",
                make_output(vec![
                    (("int", "2"), 0),
                    (("pow", "**"), 1),
                    (("int", "3"), 3),
                ]),
            ),
            (
                "-5",
                make_output(vec![(("sub", "-"), 0), (("int", "5"), 1)]),
            ),
            (
                "100 + -5",
                make_output(vec![
                    (("int", "100"), 0),
                    (("add", "+"), 4),
                    (("sub", "-"), 6),
                    (("int", "5"), 7),
                ]),
            ),
            (
                "4 - 2 ** 5 / 2",
                make_output(vec![
                    (("int", "4"), 0),
                    (("sub", "-"), 2),
                    (("int", "2"), 4),
                    (("pow", "**"), 6),
                    (("int", "5"), 9),
                    (("div", "/"), 11),
                    (("int", "2"), 13),
                ]),
            ),
            (
                "10 % 3",
                make_output(vec![
                    (("int", "10"), 0),
                    (("mod", "%"), 3),
                    (("int", "3"), 5),
                ]),
            ),
        ];

        let tok = Tokenizer::default()
            .with_literals(&[
                ("add", "+"),
                ("sub", "-"),
                ("mul", "*"),
                ("div", "/"),
                ("pow", "**"),
                ("mod", "%"),
            ])
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
                make_output(vec![(("word", "a"), 0), (("word", "b"), 2)]),
                make_output(vec![(("word", "c"), 0), (("word", "de"), 2)]),
            ]
        );
    }

    #[test]
    fn line_tokenization_fast() {
        let (a, b) = (("a", "a"), ("b", "b"));
        let tok = Tokenizer::default()
            .with_ignore_whitespace(true)
            .with_literals(&[a, b])
            .unwrap();
        let Ok(lines) = tok
            .tokenize_lines("a b\nb\ra")
            .collect::<Result<Vec<Vec<Token>>, _>>()
        else {
            panic!("tokenization failed");
        };
        assert_eq!(
            lines,
            vec![
                make_output(vec![(a, 0), (b, 2)]),
                make_output(vec![(b, 0), (a, 2)])
            ]
        );
    }

    #[test]
    fn breakpoint_tokenization() {
        let (x, y, z) = (("x", "abc"), ("y", "a"), ("z", "b"));
        let tok = Tokenizer::default().with_literals(&[x, y, z]).unwrap();
        let Ok(tokens) = tok.tokenize("ababaababc").collect::<Result<Vec<_>, _>>() else {
            panic!("tokenization failed");
        };
        assert_eq!(
            tokens,
            make_output(vec![
                (y, 0),
                (z, 1),
                (y, 2),
                (z, 3),
                (y, 4),
                (y, 5),
                (z, 6),
                (x, 7)
            ])
        );
    }

    #[test]
    fn multichar_breakpoint_tokenization() {
        let (x, y, z) = (("x", "ab"), ("y", "bc"), ("z", "abcd"));
        let tok = Tokenizer::default().with_literals(&[x, y, z]).unwrap();
        let source = "ccddbabcaabcccdcbaaabdaabcbaabbbabaaaccabcdabaabadbcacddacbddbcb";
        let tokens: Vec<_> = tok.tokenize(source).flatten().collect();
        assert_eq!(
            tokens,
            make_output(vec![
                (x, 5),
                (x, 9),
                (x, 19),
                (x, 23),
                (x, 28),
                (x, 32),
                (z, 39),
                (x, 43),
                (x, 46),
                (y, 50),
                (y, 61)
            ])
        );
    }

    #[test]
    fn fast_tokenization_with_ignoreset() {
        let (foo, bar) = (("foo", "x"), ("bar", "y"));
        let tok = Tokenizer::default()
            .with_literals(&[foo, bar])
            .unwrap()
            .with_ignored_characters(FxHashSet::from_iter(['z']));
        let Ok(tokens) = tok.tokenize("xzy").collect::<Result<Vec<_>, _>>() else {
            panic!("tokenization failed");
        };
        assert_eq!(tokens, make_output(vec![(foo, 0), (bar, 2)]));
    }

    #[test]
    fn core_tokenization_with_ignoreset() {
        let (foo, bar) = (("foo", "xz"), ("bar", "yz"));
        let tok = Tokenizer::default()
            .with_literals(&[foo, bar])
            .unwrap()
            .with_ignored_characters(FxHashSet::from_iter(['z']));
        let Ok(tokens) = tok
            .tokenize("zxzyzxzyzzzyzzxzyzzzxzz")
            .collect::<Result<Vec<_>, _>>()
        else {
            panic!("tokenization failed");
        };
        assert_eq!(
            tokens,
            make_output(vec![
                (foo, 1),
                (bar, 3),
                (foo, 5),
                (bar, 7),
                (bar, 11),
                (foo, 14),
                (bar, 16),
                (foo, 20)
            ])
        );
    }

    #[test]
    fn whitespace_tokenization() {
        let (cr, ln, space) = (("cr", "\r"), ("ln", "\n"), ("space", " "));
        let tok = Tokenizer::default()
            .with_literals(&[cr, ln, space])
            .unwrap();
        let source = " \r\n \r \n ";

        let Ok(tokens) = tok.tokenize(source).collect::<Result<Vec<_>, _>>() else {
            panic!("tokenization failed");
        };
        assert_eq!(
            tokens,
            make_output(vec![
                (space, 0),
                (cr, 1),
                (ln, 2),
                (space, 3),
                (cr, 4),
                (space, 5),
                (ln, 6),
                (space, 7)
            ])
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
                make_output(vec![(space, 0), (cr, 1)]),
                make_output(vec![(space, 0), (cr, 1), (space, 2)]),
                make_output(vec![(space, 0)]),
            ]
        );
    }

    #[test]
    fn bad_tokenization_fast() {
        let tok = Tokenizer::default();
        let Some(Err(Error::BadToken(err_value, err_position))) = tok.tokenize("x").last() else {
            panic!("tokenization didn't fail with BadToken");
        };
        assert_eq!(err_value, 'x');
        assert_eq!(err_position, 0);
    }

    #[test]
    fn bad_tokenization_core() {
        let tok = Tokenizer::default().with_literals(&[("xy", "xy")]).unwrap();
        let Some(Err(Error::BadToken(err_value, err_position))) =
            tok.tokenize("xyz").find(Result::is_err)
        else {
            panic!("tokenization didn't fail with BadToken");
        };
        assert_eq!(err_value, 'z');
        assert_eq!(err_position, 2);
    }

    #[test]
    fn word_tokenization() {
        let tok = Tokenizer::default()
            .with_patterns(vec![common::WORD.clone()])
            .unwrap();
        let tokens: Vec<_> = tok.tokenize("Hello, world!").flatten().collect();
        assert_eq!(
            tokens,
            make_output(vec![(("word", "Hello"), 0), (("word", "world"), 7)])
        );
    }

    #[test]
    fn fast_tokenization_continues_after_bad_token() {
        let (a, b) = (("a", "a"), ("b", "b"));
        let tok = Tokenizer::default().with_literals(&[a, b]).unwrap();
        let mut expected_tokens =
            make_output(vec![(a, 0), (b, 3), (b, 4), (a, 5), (a, 7)]).into_iter();
        let mut expected_error_indexes = [1, 2, 6].into_iter();

        let tokens: Vec<_> = tok.tokenize("axxbbaxa").collect();
        assert_eq!(tokens.len(), 8);

        for result in &tokens {
            match result {
                Ok(tok) => assert_eq!(tok, &expected_tokens.next().unwrap()),
                Err(Error::BadToken(c, p)) => {
                    assert_eq!((*c, *p), ('x', expected_error_indexes.next().unwrap()));
                }
                _ => panic!("unexpected error"),
            }
        }
    }

    #[test]
    fn core_tokenization_continues_after_bad_token() {
        let tok = Tokenizer::default()
            .with_literals(&[("a", "axe")])
            .unwrap()
            .with_patterns([("b".into(), "box".into())].into())
            .unwrap();
        let expected_tokens = [
            ("a", "axe", 0),
            ("b", "box", 4),
            ("a", "axe", 8),
            ("a", "axe", 12),
            ("b", "box", 16),
            ("b", "box", 20),
        ];
        let expected_errors = ['&', ',', ',', '.', '.'];

        let tokens: Vec<_> = tok.tokenize("axe&box,axe,axe.box.box").collect();
        assert_eq!(tokens.len(), 11);

        for i in (0..tokens.len()).step_by(2) {
            assert_eq!(
                tokens[i].as_ref().unwrap(),
                &Token::from(expected_tokens[i / 2])
            );
        }
        for i in (1..tokens.len()).step_by(2) {
            let expected_err = (expected_errors[i / 2], i * 2 + 1);
            assert!(matches!(tokens[i], Err(Error::BadToken(c, p)) if (c, p) == expected_err));
        }
    }

    #[test]
    fn backreference_pattern() {
        let tok = Tokenizer::default()
            .with_patterns(vec![("a".into(), r"(a)b\1".into())])
            .unwrap();
        let out: Vec<_> = tok.tokenize("abaaba").flatten().collect();
        assert_eq!(out, make_output(vec![(("a", "aba"), 0), (("a", "aba"), 3)]));
    }

    #[test]
    fn duplicate_literal_names() {
        let (a, b) = (("a", "a"), ("a", "b"));
        let tok = Tokenizer::default().with_literals(&[a, b]).unwrap();
        let tokens: Vec<_> = tok.tokenize("ab").flatten().collect();
        assert_eq!(tokens, make_output(vec![(a, 0), (b, 1)]));
    }
}
