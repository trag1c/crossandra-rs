use std::collections::HashSet;

use regex::Regex;

use crate::error::Error;

pub(crate) fn validate(patterns: Vec<(String, String)>) -> Result<Vec<(String, String)>, Error> {
    let mut names: HashSet<&String> = HashSet::new();
    for (name, _) in &patterns {
        if !names.insert(name) {
            return Err(Error::DuplicatePattern(name.clone()));
        }
    }
    Ok(patterns)
}

pub(crate) fn compile(patterns: Vec<(String, String)>) -> Result<Vec<(String, Regex)>, Error> {
    patterns
        .into_iter()
        .map(|(key, val)| {
            Regex::new(&val)
                .map(|regex| (key, regex))
                .map_err(Error::InvalidRegex)
        })
        .collect()
}

pub(crate) fn adjust(patterns: Vec<(String, String)>) -> Vec<(String, String)> {
    let pat = Regex::new(r"\^").unwrap();
    patterns
        .into_iter()
        .map(|(name, pattern)| {
            let mut indices = HashSet::<usize>::new();

            for c in pat.find_iter(&pattern) {
                let i = c.start();
                if i == 0 {
                    indices.insert(0);
                } else if let Some(c) = pattern.chars().nth(i - 1) {
                    if !matches!(c, '[' | '\\') {
                        indices.insert(i);
                    }
                }
            }

            let adjusted: String = pattern
                .chars()
                .enumerate()
                .filter(|(i, _)| !indices.contains(i))
                .map(|(_, char)| char)
                .collect();

            (name, format!("^({adjusted})"))
        })
        .collect()
}
