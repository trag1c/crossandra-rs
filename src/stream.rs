use std::str::CharIndices;

use fancy_regex::Regex;
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

    fn try_match_literals(&self) -> Option<(&'a str, &'a str)> {
        self.tokenizer
            .tree
            .match_longest_prefix(self.remaining_source)
            .map(|(prefix, name)| (name, prefix))
    }

    fn try_match(&self, name: &'a str, pattern: &Regex) -> Option<(&'a str, &'a str)> {
        pattern
            .find(self.remaining_source)
            .ok()
            .flatten()
            .map(|tok| (name, tok.as_str()))
    }

    fn try_match_patterns(&self) -> Option<(&'a str, &'a str)> {
        self.tokenizer
            .patterns
            .iter()
            .find_map(|(name, pattern)| self.try_match(name, pattern))
    }
}

impl<'a> Iterator for Core<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        // Skip ignored characters
        let (index, _) = self
            .remaining_source
            .char_indices()
            .find(|(_, c)| !self.ignored.contains(c))?;

        self.remaining_source = &self.remaining_source[index..];
        self.position += index;

        // Try to match tokens
        let matching_result = self
            .try_match_literals()
            .or_else(|| self.try_match_patterns());

        let (result, step) = matching_result.map_or_else(
            || {
                let next_char = self
                    .remaining_source
                    .chars()
                    .next()
                    .expect("the chunk will never be empty");

                (
                    Err(Error::BadToken(next_char, self.position)),
                    next_char.len_utf8(),
                )
            },
            |(name, value)| {
                (
                    Ok(Token {
                        name,
                        value,
                        position: self.position,
                    }),
                    value.len(),
                )
            },
        );

        self.remaining_source = &self.remaining_source[step..];
        self.position += step;

        Some(result)
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
