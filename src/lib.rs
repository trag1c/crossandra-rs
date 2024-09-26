use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    iter::{once, Chain, Once, Take},
    str::Chars,
};

use regex::Regex;

const WHITESPACE: [char; 6] = [' ', '\x0c', '\t', '\x0b', '\r', '\n'];

#[derive(Debug, Clone)]
enum Tree {
    Leaf(String),
    Node(HashMap<Option<char>, Tree>),
}

#[derive(Debug)]
pub struct Token {
    pub name: String,
    pub value: String,
}

#[derive(Debug)]
pub struct TokenizationError(char);

impl Display for TokenizationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "invalid token {:?}", self.0)
    }
}

pub struct Crossandra<'a> {
    literals: HashMap<&'a str, &'a str>,
    patterns: Vec<(String, Regex)>,
    convert_crlf: bool,
    ignore_whitespace: bool,
    ignored_characters: Vec<char>,
    suppress_unknown: bool,
    tree: Tree,
}

impl<'a> Crossandra<'a> {
    pub fn new(
        literals: HashMap<&'a str, &'a str>,
        patterns: Vec<(String, String)>,
        ignored_characters: Vec<char>,
        convert_crlf: bool,
        ignore_whitespace: bool,
        suppress_unknown: bool,
    ) -> Result<Self, regex::Error> {
        let literals = flip_hashmap(literals);
        Ok(Self {
            tree: generate_tree(&literals),
            literals,
            patterns: compile_patterns(patterns)?,
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

    pub fn tokenize(&self, source: &str) -> Result<Vec<Token>, TokenizationError> {
        let source = self.prepare_source(source);
        let ignored = self.prepare_ignored();
        if self.can_use_fast_mode() {
            self.tokenize_fast(&source, &ignored, &self.prepare_literal_map())
        } else {
            self.tokenize_core(&source, &ignored)
        }
    }

    pub fn tokenize_lines(&self, lines: Vec<&str>) -> Result<Vec<Vec<Token>>, ()> {
        todo!("tokenize_lines")
    }

    fn tokenize_core(
        &self,
        source: &str,
        ignored_characters: &HashSet<char>,
    ) -> Result<Vec<Token>, TokenizationError> {
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

            let remaining_source = &chars.clone().collect::<String>();
            let mut applied_rule = false;
            for (name, pattern) in &self.patterns {
                if let Some(tok) = pattern.find(remaining_source) {
                    let value = tok;
                    tokens.push(Token {
                        name: name.clone(),
                        value: value.as_str().to_string(),
                    });
                    for _ in 0..value.len() - 1 {
                        chars.next();
                    }
                    applied_rule = true;
                    break;
                }
            }

            if !(applied_rule || self.suppress_unknown) {
                return Err(TokenizationError(handling_result.unwrap_err()));
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
                        return Ok((token_name.to_string(), joined_chunk[..i].to_string(), i + 1))
                    }
                    Some(new_tree) => tree = new_tree,
                    None => match node.get(&None) {
                        None => break,
                        Some(Tree::Leaf(token_name)) => {
                            return Ok((token_name.to_string(), joined_chunk[..i].to_string(), i))
                        }
                        _ => unreachable!("key None can never lead to a Node"),
                    },
                }
            }
        }

        match break_path {
            // TODO: replace String::new() with a self.literals fetch
            Some((s, u)) => Ok((String::new(), s.to_string(), u)),
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
    ) -> Result<Vec<Token>, TokenizationError> {
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
                        return Err(TokenizationError(char));
                    }
                }
            }
        }
        Ok(tokens)
    }

    #[must_use]
    pub fn with_literals(mut self, literals: HashMap<&'a str, &'a str>) -> Self {
        self.literals = flip_hashmap(literals);
        self.tree = generate_tree(&self.literals);
        self
    }

    #[must_use]
    pub fn with_patterns(mut self, patterns: Vec<(String, String)>) -> Result<Self, regex::Error> {
        self.patterns = compile_patterns(patterns)?;
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

    pub fn set_literals(&mut self, literals: HashMap<&'a str, &'a str>) {
        self.literals = flip_hashmap(literals);
        self.tree = generate_tree(&self.literals);
    }

    pub fn set_patterns(&mut self, patterns: Vec<(String, String)>) -> Result<(), regex::Error> {
        self.patterns = compile_patterns(patterns)?;
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

impl<'a> Default for Crossandra<'a> {
    fn default() -> Self {
        Self::new(HashMap::new(), Vec::new(), Vec::new(), true, false, false).unwrap()
    }
}

fn generate_tree(literals: &HashMap<&str, &str>) -> Tree {
    let mut sorted_items: Vec<_> = literals.iter().collect();
    sorted_items.sort_by(|(k1, _), (k2, _)| k2.len().cmp(&k1.len()));

    let mut root = Tree::Node(HashMap::new());

    for (k, v) in sorted_items {
        let mut current = &mut root;

        for c in k[..k.len() - 1].chars() {
            if let Tree::Node(ref mut map) = current {
                current = map.entry(Some(c)).or_insert(Tree::Node(HashMap::new()));
            }
        }

        if let Tree::Node(map) = current {
            let last_char = k.chars().last().expect("Key should not be empty");
            let token_name = Tree::Leaf((*v).to_string());
            match map.get_mut(&Some(last_char)) {
                Some(inner_tree) => {
                    if let Tree::Node(node) = inner_tree {
                        node.insert(None, token_name);
                    }
                }
                None => {
                    map.insert(Some(last_char), token_name);
                }
            };
        }
    }
    root
}

fn flip_hashmap<'a>(hm: HashMap<&'a str, &'a str>) -> HashMap<&'a str, &'a str> {
    hm.into_iter().map(|(k, v)| (v, k)).collect()
}

fn compile_patterns(hm: Vec<(String, String)>) -> Result<Vec<(String, Regex)>, regex::Error> {
    hm.into_iter()
        .map(|(key, val)| Regex::new(&val).map(|regex| (key, regex)))
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::{flip_hashmap, generate_tree, Crossandra};
    use std::collections::HashMap;

    #[test]
    fn fast_mode_test() {
        let tok = Crossandra::default()
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
