use std::str::CharIndices;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{error::Error, Token, Tokenizer};

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
    remaining_source: &'a str,
    ignored: FxHashSet<char>,
    position: usize,
}

impl<'a> Core<'a> {
    pub fn new(tok: &'a Tokenizer<'a>, source: &'a str, ignored: FxHashSet<char>) -> Self {
        Self {
            tokenizer: tok,
            remaining_source: source,
            ignored,
            position: 0,
        }
    }

    fn handle(&self, remaining_source: &'a str) -> Result<(&'a str, &'a str, usize), char> {
        self.tokenizer
            .tree
            .match_longest_prefix(remaining_source)
            .map(|(prefix, name)| (name, prefix, prefix.len()))
            .ok_or_else(|| {
                remaining_source
                    .chars()
                    .next()
                    .expect("the chunk will never be empty")
            })
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

        let handling_result = self.handle(self.remaining_source);

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
