use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    iter::{once, Chain, Once, Take},
    str::Chars,
};

use regex::Regex;

mod error;
use error::Error;

mod tree;
use tree::{generate_tree, Tree};

const WHITESPACE: [char; 6] = [' ', '\x0c', '\t', '\x0b', '\r', '\n'];

#[derive(Debug)]
pub struct Token {
    pub name: String,
    pub value: String,
}

pub struct Tokenizer<'a> {
    literals: HashMap<&'a str, &'a str>,
    patterns: Vec<(String, Regex)>,
    convert_crlf: bool,
    ignore_whitespace: bool,
    ignored_characters: Vec<char>,
    suppress_unknown: bool,
    tree: Tree,
}

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
            patterns: compile_patterns(validate_patterns(patterns)?)?,
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
            .map(|(k, v)| (k.chars().next().unwrap(), *v))
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

    pub fn tokenize_lines(&self, lines: Vec<&str>) -> Result<Vec<Vec<Token>>, Error> {
        let ignored = self.prepare_ignored();
        if self.can_use_fast_mode() {
            let literal_map = &self.prepare_literal_map();
            lines
                .iter()
                .map(|line| self.tokenize_fast(line, &ignored, literal_map))
                .collect()
        } else {
            lines
                .iter()
                .map(|line| self.tokenize_core(line, &ignored))
                .collect()
        }
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
                if let Some(tok) = pattern.find(&remaining_source) {
                    tokens.push(Token {
                        name: name.clone(),
                        value: tok.as_str().to_string(),
                    });
                    for _ in 0..tok.len() - 1 {
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

    #[must_use]
    pub fn with_literals(mut self, literals: HashMap<&'a str, &'a str>) -> Result<Self, Error> {
        self.literals = flip_hashmap(validate_literals(literals)?);
        self.tree = generate_tree(&self.literals);
        Ok(self)
    }

    #[must_use]
    pub fn with_patterns(mut self, patterns: Vec<(String, String)>) -> Result<Self, Error> {
        self.patterns = compile_patterns(validate_patterns(patterns)?)?;
        println!("with_patterns {:?}", self.patterns);
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
        self.patterns = compile_patterns(validate_patterns(patterns)?)?;
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
        Self::new(HashMap::new(), Vec::new(), Vec::new(), true, false, false).unwrap()
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

fn validate_patterns(patterns: Vec<(String, String)>) -> Result<Vec<(String, String)>, Error> {
    let mut names: HashSet<&String> = HashSet::new();
    for (name, _) in &patterns {
        if !names.insert(name) {
            return Err(Error::DuplicatePattern(name.clone()));
        }
    }
    Ok(patterns)
}

fn compile_patterns(hm: Vec<(String, String)>) -> Result<Vec<(String, Regex)>, Error> {
    hm.into_iter()
        .map(|(key, val)| {
            Regex::new(&val)
                .map(|regex| (key, regex))
                .map_err(Error::InvalidRegex)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::{flip_hashmap, generate_tree, Error, Tokenizer};
    use std::collections::HashMap;

    #[test]
    fn pattern_validation_err() {
        let tok = Tokenizer::default()
            .with_patterns(vec![("foo".into(), "x".into()), ("foo".into(), "y".into())]);
        assert!(matches!(tok, Err(Error::DuplicatePattern(s)) if s == *"foo"));
    }

    #[test]
    fn pattern_validation_ok() {
        let tok = Tokenizer::default()
            .with_patterns(vec![("foo".into(), "x".into()), ("fxx".into(), "y".into())]);
        assert!(tok.is_ok());
    }

    #[test]
    fn literal_validation_err() {
        let tok = Tokenizer::default().with_literals(HashMap::from_iter([("x", "x"), ("y", "")]));
        assert!(matches!(tok, Err(Error::EmptyLiteral)));
    }

    #[test]
    fn literal_validation_ok() {
        let tok = Tokenizer::default().with_literals(HashMap::from_iter([("x", "x"), ("", "y")]));
        assert!(tok.is_ok());
    }

    #[test]
    fn fast_mode_test() {
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
            .unwrap()
            .with_suppress_unknown(true);
        // println!("{:?}", tok.literals);
        // assert!(tok.can_use_fast_mode());
        let tokens = tok.tokenize("cat program: ,[.,]").unwrap();
        for tok in tokens {
            println!("{tok:?}");
        }
    }

    #[test]
    fn generate_tree_test() {
        let hm: HashMap<&str, &str> = HashMap::from_iter([
            // ("add", "+"),
            // ("sub", "-"),
            // ("left", "<"),
            // ("right", ">"),
            // ("read", ","),
            // ("write", "."),
            // ("begin_loop", "["),
            // ("end_loop", "]"),
            ("add", "+"),
            ("sub", "-"),
            ("mul", "++"),
            ("div", "--"),
            ("pow", "+++"),
            ("mod", "---"),
            ("ge", ">:"),
            ("gt", ">"),
            ("le", "<:"),
            ("lt", "<"),
            ("eq", "::"),
            ("ne", ":::"),
            ("and", "&&"),
            ("in", "->?"),
            ("not", "~~"),
            ("or", "||"),
            ("xor", "^^"),
            ("band", "&"),
            ("bnot", "~"),
            ("bor", "|"),
            ("bxor", "^"),
            ("bracket_open", "["),
            ("bracket_close", "]"),
            ("brace_open", "{"),
            ("brace_close", "}"),
            ("paren_open", "("),
            ("paren_close", ")"),
            ("table_open", "{{"),
            ("table_close", "}}"),
            ("catch", "!!"),
            ("else", ",,"),
            ("for", "..."),
            ("from", "<-"),
            ("if", "?"),
            ("import", "<="),
            ("throw", "!!!"),
            ("to", "->"),
            ("try", "??"),
            ("while", ".."),
            ("class", "@"),
            ("dataclass", "@!"),
            ("default", "<>"),
            ("function", "*"),
            ("instance", "'"),
            ("entry", "=>"),
            ("yield", "**"),
            ("slice_open", "<<"),
            ("slice_close", ">>"),
            ("cast", "%"),
            ("special", "$"),
            ("exit", "=>!"),
            ("hash", "##"),
            ("parent", "!?"),
            ("type", "?!"),
            ("readline", "???"),
            ("print", "!"),
            ("file_create", "?~>"),
            ("file_append", "&~~>"),
            ("file_read", "<~~"),
            ("file_write", "~~>"),
            ("file_read_write", "<~>"),
            ("file_binary_append", "&%~>"),
            ("file_binary_read", "<~%"),
            ("file_binary_write", "%~>"),
            ("file_binary_read_write", "<%>"),
            ("file_quick_append", "&~>"),
            ("file_quick_read", "<~"),
            ("file_quick_write", "~>"),
            ("file_quick_binary_append", "&%>"),
            ("file_quick_binary_read", "<%"),
            ("file_quick_binary_write", "%>"),
            ("enum", "#"),
            ("assign", ":"),
            ("attr", "."),
            ("unix_stmp", "@@"),
            ("arr_stmp", "@@@"),
            ("end", ";"),
            ("sep", ","),
            ("sleep", ",.,"),
            ("zip", "><"),
        ]);
        let fhm = flip_hashmap(hm);
        let tree = generate_tree(&fhm);
        println!("{tree:?}");
        // panic!();
    }
}
