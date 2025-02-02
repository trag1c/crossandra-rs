use std::borrow::Cow;

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
        remaining_source: &str,
        chunk_size: usize,
    ) -> Result<(Cow<'a, str>, String, usize), char> {
        let mut break_path = None;
        let mut tree = &self.tokenizer.tree;

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
                        Cow::Borrowed(token_name),
                        remaining_source[..=i].to_string(),
                        i + 1,
                    ));
                }
                Some(new_tree) => tree = new_tree,
                None => match node.get(&None) {
                    None => break,
                    Some(Tree::Leaf(token_name)) => {
                        return Ok((
                            Cow::Borrowed(token_name),
                            remaining_source[..i].to_string(),
                            i,
                        ));
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
                    return Ok((Cow::Borrowed(token_name), joined_chunk, joined_chunk_len));
                }
                _ => unreachable!("key None can never lead to a Node"),
            }
        }

        if let Some((s, len)) = break_path {
            return Ok((
                Cow::Borrowed(s),
                remaining_source.chars().take(len).collect(),
                len,
            ));
        }

        if let Some(&name) = self.tokenizer.literals.get(joined_chunk.as_str()) {
            Ok((Cow::Borrowed(name), joined_chunk, joined_chunk_len))
        } else {
            Err(joined_chunk
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

        let handling_result = self.handle(self.remaining_source, self.chunk_size);

        if let Ok((name, value, size)) = handling_result {
            self.remaining_source = &self.remaining_source[size..];
            self.position += index + size;
            return Some(Ok(Token {
                name,
                value,
                position: self.position - size,
            }));
        }

        for (name, pattern) in &self.tokenizer.patterns {
            let Ok(Some(tok)) = pattern.find(self.remaining_source) else {
                continue;
            };
            self.remaining_source = &self.remaining_source[tok.end()..];
            let size = tok.end() - tok.start();
            self.position += index + size;
            return Some(Ok(Token {
                name: Cow::Borrowed(name),
                value: tok.as_str().to_string(),
                position: self.position - size,
            }));
        }

        let char = handling_result.unwrap_err();
        self.remaining_source = &self.remaining_source[char.len_utf8()..];
        self.position += 1;
        Some(Err(Error::BadToken(char, self.position - 1)))
    }
}

pub(crate) struct Fast<'a, I> {
    literal_map: FxHashMap<char, &'a str>,
    ignored: FxHashSet<char>,
    chars: I,
    position: usize,
}

impl<'a, I> Fast<'a, I>
where
    I: Iterator<Item = char>,
{
    pub fn new(tok: &'a Tokenizer<'a>, chars: I, ignored: FxHashSet<char>) -> Self {
        Self {
            chars,
            ignored,
            literal_map: prepare_literal_map(tok),
            position: 0,
        }
    }
}

impl<'a, I> Iterator for Fast<'a, I>
where
    I: Iterator<Item = char>,
{
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let char = self.chars.find(|c| {
            self.position += 1;
            !self.ignored.contains(c)
        })?;
        match self.literal_map.get(&char) {
            Some(&name) => Some(Ok(Token {
                name: Cow::Borrowed(name),
                value: char.to_string(),
                position: self.position - 1,
            })),
            None => Some(Err(Error::BadToken(char, self.position - 1))),
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
