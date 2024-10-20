use std::{
    collections::{HashMap, HashSet},
    iter::{once, Chain, Once, Take},
    str::Chars,
};

use fancy_regex::Regex;

pub mod common;

mod error;
use error::Error;

mod tree;
use tree::{generate_tree, Tree};

mod patterns;

const WHITESPACE: [char; 6] = [' ', '\x0c', '\t', '\x0b', '\r', '\n'];

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub name: String,
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
    pub fn new(
        literals: HashMap<&'a str, &'a str>,
        patterns: Vec<(String, String)>,
        ignored_characters: Vec<char>,
        convert_crlf: bool,
        ignore_whitespace: bool,
        suppress_unknown: bool,
    ) -> Result<Self, Error> {
        let literals = flip_hashmap(validate_literals(literals)?);
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
            .map(|(k, v)| (k.chars().next().expect("all literals should be 1-long"), *v))
            .collect()
    }

    pub fn tokenize(&self, source: &str) -> Result<Vec<Token>, Error> {
        let source = self.prepare_source(source);
        let ignored = self.prepare_ignored();
        if self.can_use_fast_mode() {
            self.tokenize_fast(&source, &ignored, &self.prepare_literal_map())
        } else {
            self.tokenize_core(&source, &ignored)
        }
    }

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
        let mut tokens: Vec<Token> = Vec::new();
        let mut chars = source.chars();
        let chunk_size = self.literals.keys().map(|x| x.len()).max().unwrap_or(1);

        while let Some(c) = &chars.next() {
            if ignored_characters.contains(c) {
                continue;
            }
            let handling_result = self.handle(once(*c).chain(chars.clone().take(chunk_size - 1)));
            if let Ok((name, value, size)) = handling_result {
                tokens.push(Token { name, value });
                for _ in 0..size - 1 {
                    chars.next();
                }
                continue;
            }

            let remaining_source = once(*c).chain(chars.clone()).collect::<String>();
            let mut applied_rule = false;
            for (name, pattern) in &self.patterns {
                if let Ok(Some(tok)) = pattern.find(&remaining_source) {
                    tokens.push(Token {
                        name: name.clone(),
                        value: tok.as_str().to_string(),
                    });
                    for _ in 0..tok.end() - 1 {
                        chars.next();
                    }
                    applied_rule = true;
                    break;
                }
            }

            if !(applied_rule || self.suppress_unknown) {
                return Err(Error::BadToken(handling_result.unwrap_err()));
            }
        }
        Ok(tokens)
    }

    fn handle(
        &self,
        chunk: Chain<Once<char>, Take<Chars>>,
    ) -> Result<(String, String, usize), char> {
        let mut break_path: Option<(&String, usize)> = None;
        let mut tree = &self.tree;
        let joined_chunk: String = chunk.clone().collect();

        for (i, v) in chunk.enumerate() {
            if let Tree::Node(ref node) = tree {
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
                            joined_chunk[..=i].to_string(),
                            i + 1,
                        ));
                    }
                    Some(new_tree) => tree = new_tree,
                    None => match node.get(&None) {
                        None => break,
                        Some(Tree::Leaf(token_name)) => {
                            return Ok((token_name.to_string(), joined_chunk[..i].to_string(), i));
                        }
                        _ => unreachable!("key None can never lead to a Node"),
                    },
                }
            }
        }

        if let Tree::Node(ref node) = tree {
            if let Some(t) = node.get(&None) {
                if let Tree::Leaf(token_name) = t {
                    break_path = Some((token_name, joined_chunk.len()));
                } else {
                    unreachable!("key None can never lead to a Node")
                }
            };

            match node.get(&None) {
                None => {}
                Some(Tree::Leaf(token_name)) => {
                    return Ok((
                        token_name.to_string(),
                        joined_chunk.to_string(),
                        joined_chunk.len(),
                    ));
                }
                _ => unreachable!("key None can never lead to a Node"),
            }
        }

        if let Some((s, u)) = break_path {
            return Ok((
                s.to_string(),
                if let Some(value) = flip_hashmap(self.literals.clone()).get(&s.as_str()) {
                    (*value).to_string()
                } else {
                    return Err(s.chars().next().expect("the token will never be unnamed"));
                },
                u,
            ));
        }

        let chunk_len = joined_chunk.len();
        match self.literals.get(&joined_chunk.as_str()) {
            Some(name) => Ok(((*name).to_string(), joined_chunk, chunk_len)),
            None => Err(joined_chunk
                .chars()
                .next()
                .expect("the chunk will never be empty")),
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

    pub fn with_literals(mut self, literals: HashMap<&'a str, &'a str>) -> Result<Self, Error> {
        self.literals = flip_hashmap(validate_literals(literals)?);
        self.tree = generate_tree(&self.literals);
        Ok(self)
    }

    pub fn with_patterns(mut self, patterns: Vec<(String, String)>) -> Result<Self, Error> {
        self.patterns = patterns::prepare(patterns)?;
        Ok(self)
    }

    #[must_use]
    pub fn with_ignored_characters(mut self, ignored_characters: Vec<char>) -> Self {
        self.ignored_characters = ignored_characters;
        self
    }

    #[must_use]
    pub fn with_convert_crlf(mut self, convert_crlf: bool) -> Self {
        self.convert_crlf = convert_crlf;
        self
    }

    #[must_use]
    pub fn with_ignore_whitespace(mut self, ignore_whitespace: bool) -> Self {
        self.ignore_whitespace = ignore_whitespace;
        self
    }

    #[must_use]
    pub fn with_suppress_unknown(mut self, suppress_unknown: bool) -> Self {
        self.suppress_unknown = suppress_unknown;
        self
    }

    pub fn set_literals(&mut self, literals: HashMap<&'a str, &'a str>) -> Result<(), Error> {
        self.literals = flip_hashmap(validate_literals(literals)?);
        self.tree = generate_tree(&self.literals);
        Ok(())
    }

    pub fn set_patterns(&mut self, patterns: Vec<(String, String)>) -> Result<(), Error> {
        self.patterns = patterns::prepare(patterns)?;
        Ok(())
    }

    pub fn set_ignored_characters(&mut self, ignored_characters: Vec<char>) {
        self.ignored_characters = ignored_characters;
    }

    pub fn set_convert_crlf(&mut self, convert_crlf: bool) {
        self.convert_crlf = convert_crlf;
    }

    pub fn set_ignore_whitespace(&mut self, ignore_whitespace: bool) {
        self.ignore_whitespace = ignore_whitespace;
    }

    pub fn set_suppress_unknown(&mut self, suppress_unknown: bool) {
        self.suppress_unknown = suppress_unknown;
    }
}

impl<'a> Default for Tokenizer<'a> {
    fn default() -> Self {
        Self::new(HashMap::new(), Vec::new(), Vec::new(), true, false, false)
            .expect("an empty tokenizer should be correct")
    }
}

fn flip_hashmap<'a>(hm: HashMap<&'a str, &'a str>) -> HashMap<&'a str, &'a str> {
    hm.into_iter().map(|(k, v)| (v, k)).collect()
}

fn validate_literals<'a>(
    literals: HashMap<&'a str, &'a str>,
) -> Result<HashMap<&'a str, &'a str>, Error> {
    if literals.values().any(|lit| lit.is_empty()) {
        return Err(Error::EmptyLiteral);
    }
    Ok(literals)
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
            validate_literals(HashMap::from([("x", "x"), ("y", "")])),
            Err(Error::EmptyLiteral)
        ));
    }

    #[test]
    fn literal_validation_ok() {
        assert!(validate_literals(HashMap::from([("x", "x"), ("", "y")])).is_ok());
    }

    #[test]
    fn flip_hashmap_ok() {
        assert_eq!(flip_hashmap(HashMap::new()), HashMap::new());
        assert_eq!(
            flip_hashmap(HashMap::from([("a", "b"), ("c", "d")])),
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
                .with_literals(HashMap::from([("x", "a"), ("y", "b")]))
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
                .with_literals([("1", "2")].into())
                .unwrap()
        );
        assert_eq!(
            def.clone().with_literals([("1", "2")].into()).unwrap(),
            Tokenizer::default()
                .with_literals([("1", "2")].into())
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
        tok1.set_literals(literals.clone()).unwrap();
        tok1.set_patterns(patterns.clone()).unwrap();

        let tok2 = Tokenizer::default()
            .with_convert_crlf(false)
            .with_ignore_whitespace(true)
            .with_suppress_unknown(true)
            .with_ignored_characters(ignored_chars.clone())
            .with_literals(literals.clone())
            .unwrap()
            .with_patterns(patterns.clone())
            .unwrap();

        let tok3 = Tokenizer::new(literals, patterns, ignored_chars, false, true, true).unwrap();

        assert_eq!(tok1, tok2);
        assert_eq!(tok1, tok3);
        assert_eq!(tok2, tok3);
    }

    #[test]
    fn builder_processing_literals() {
        let mut tok = Tokenizer::default();
        assert_eq!(tok.tree, Tree::Node(HashMap::new()));

        let literals = HashMap::from([("a", "b")]);
        assert!(tok.set_literals(literals).is_ok());

        let flipped_literals = HashMap::from([("b", "a")]);
        let expected_tree = generate_tree(&flipped_literals);
        assert_eq!(tok.literals, flipped_literals);
        assert_eq!(tok.tree, expected_tree);

        assert!(tok.set_literals(HashMap::from([("a", "")])).is_err());
    }

    #[test]
    fn builder_processing_patterns() {
        let mut tok = Tokenizer::default();
        assert!(tok.patterns.is_empty());

        let pattern = ("a".into(), r"\d+".into());
        assert!(tok.set_patterns(vec![pattern.clone()]).is_ok());
        assert_eq!(tok.patterns.first().unwrap().1.as_str(), r"^(\d+)");

        assert!(tok.set_patterns(vec![pattern.clone(), pattern]).is_err());
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
            .with_literals(HashMap::from_iter([
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
                [
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
            .with_literals([x, y, z].into())
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
            .with_literals(literals.into())
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
            .with_literals([foo, bar].into())
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
            .with_literals([cr, ln, space].into())
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
            .with_literals([("xy", "xy")].into())
            .unwrap();
        let Error::BadToken(bad_token) = tok.tokenize("xyz").unwrap_err() else {
            panic!("tokenization didn't fail with BadToken");
        };
        assert_eq!(bad_token, 'z');
    }
}
