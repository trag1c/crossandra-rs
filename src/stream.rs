use std::str::CharIndices;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{error::Error, tree::Tree, Token, Tokenizer};

pub(crate) fn build_hashmap<'a>(hm: &[(&'a str, &'a str)]) -> FxHashMap<&'a str, &'a str> {
    hm.iter().map(|(k, v)| (*v, *k)).collect()
}

fn prepare_literal_map<'a>(tok: &'a Tokenizer) -> FxHashMap<char, &'a str> {
    tok.literals
        .iter()
        .map(|(&k, &v)| (k.chars().next().expect("all literals should be 1-long"), v))
        .collect()
}

pub(crate) struct Core<'a> {
    tokenizer: &'a Tokenizer<'a>,
    chunk_size: usize,
    remaining_source: &'a str,
    ignored: FxHashSet<char>,
    position: usize,
}

impl<'a> Core<'a> {
    pub fn new(tok: &'a Tokenizer<'a>, source: &'a str, ignored: FxHashSet<char>) -> Self {
        Self {
            tokenizer: tok,
            chunk_size: tok.literals.keys().map(|x| x.len()).max().unwrap_or(1),
            remaining_source: source,
            ignored,
            position: 0,
        }
    }

    fn handle(
        &self,
        remaining_source: &'a str,
        chunk_size: usize,
    ) -> Result<(&'a str, &'a str, usize), char> {
        let mut break_path = None;
        let mut tree = &self.tokenizer.tree;

        for (i, v) in remaining_source.char_indices().take(chunk_size) {
            let Tree::Node(ref node) = tree else { continue };

            if let Some(token) = node.get(&None) {
                if let Tree::Leaf(token_name) = token {
                    break_path = Some((token_name, i));
                } else {
                    unreachable!("key None can never lead to a Node")
                }
            };

            match node.get(&Some(v)) {
                Some(Tree::Leaf(token_name)) => {
                    let next_char_index = i + v.len_utf8();
                    return Ok((
                        token_name,
                        (&remaining_source[..next_char_index]),
                        next_char_index,
                    ));
                }
                Some(new_tree) => tree = new_tree,
                None => match node.get(&None) {
                    None => break,
                    Some(Tree::Leaf(token_name)) => {
                        return Ok(((token_name), (&remaining_source[..i]), i));
                    }
                    _ => unreachable!("key None can never lead to a Node"),
                },
            }
        }

        let chunk_length = remaining_source
            .chars()
            .take(chunk_size)
            .map(char::len_utf8)
            .sum();

        let chunk = &remaining_source[..chunk_length];

        if let Tree::Node(ref node) = tree {
            if let Some(t) = node.get(&None) {
                if let Tree::Leaf(token_name) = t {
                    break_path = Some((token_name, chunk_length));
                } else {
                    unreachable!("key None can never lead to a Node")
                }
            };

            match node.get(&None) {
                None => {}
                Some(Tree::Leaf(token_name)) => {
                    return Ok(((token_name), (chunk), chunk_length));
                }
                _ => unreachable!("key None can never lead to a Node"),
            }
        }

        if let Some((s, len)) = break_path {
            let bytes = remaining_source.chars().take(len).map(char::len_utf8).sum();
            return Ok(((s), (&remaining_source[..bytes]), len));
        }

        if let Some(&name) = self.tokenizer.literals.get(chunk) {
            Ok(((name), (chunk), chunk_length))
        } else {
            Err(remaining_source
                .chars()
                .next()
                .expect("the chunk will never be empty"))
        }
    }
}

impl<'a> Iterator for Core<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let (index, _) = self
            .remaining_source
            .char_indices()
            .find(|(_, c)| !self.ignored.contains(c))?;

        self.remaining_source = &self.remaining_source[index..];
        self.position += index;
        let start_position = self.position;

        let handling_result = self.handle(self.remaining_source, self.chunk_size);

        if let Ok((name, value, size)) = handling_result {
            self.remaining_source = &self.remaining_source[size..];
            self.position += size;
            return Some(Ok(Token {
                name,
                value,
                position: start_position,
            }));
        }

        for (name, pattern) in &self.tokenizer.patterns {
            let Ok(Some(tok)) = pattern.find(self.remaining_source) else {
                continue;
            };
            self.remaining_source = &self.remaining_source[tok.end()..];
            self.position += tok.end() - tok.start();
            return Some(Ok(Token {
                name,
                value: tok.as_str(),
                position: start_position,
            }));
        }

        let char = handling_result.unwrap_err();
        let char_bytes = char.len_utf8();
        self.remaining_source = &self.remaining_source[char_bytes..];
        self.position += char_bytes;
        Some(Err(Error::BadToken(char, start_position)))
    }
}

pub(crate) struct Fast<'a> {
    literal_map: FxHashMap<char, &'a str>,
    ignored: FxHashSet<char>,
    source: &'a str,
    char_indices: CharIndices<'a>,
}

impl<'a> Fast<'a> {
    pub fn new(tok: &'a Tokenizer<'a>, source: &'a str, ignored: FxHashSet<char>) -> Self {
        Self {
            source,
            char_indices: source.char_indices(),
            ignored,
            literal_map: prepare_literal_map(tok),
        }
    }
}

impl<'a> Iterator for Fast<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let (index, char) = self
            .char_indices
            .find(|&(_, c)| !self.ignored.contains(&c))?;

        match self.literal_map.get(&char) {
            Some(&name) => Some(Ok(Token {
                name,
                value: &self.source[index..index + char.len_utf8()],
                position: index,
            })),
            None => Some(Err(Error::BadToken(char, index))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flip_hashmap_ok() {
        assert_eq!(build_hashmap(&[]), FxHashMap::default());
        assert_eq!(
            build_hashmap(&[("a", "b"), ("c", "d")]),
            FxHashMap::from_iter([("b", "a"), ("d", "c")])
        );
    }

    #[test]
    fn literal_map_preparation() {
        assert_eq!(
            prepare_literal_map(
                &Tokenizer::default()
                    .with_literals(&[("x", "a"), ("y", "b")])
                    .unwrap()
            ),
            FxHashMap::from_iter([('a', "x"), ('b', "y")])
        );
    }
}
