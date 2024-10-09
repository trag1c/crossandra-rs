use std::collections::HashSet;

use regex::Regex;

use crate::error::Error;

pub(crate) fn prepare(patterns: Vec<(String, String)>) -> Result<Vec<(String, Regex)>, Error> {
    compile(validate(adjust(patterns))?)
}

fn validate(patterns: Vec<(String, String)>) -> Result<Vec<(String, String)>, Error> {
    let mut names: HashSet<&String> = HashSet::new();
    for (name, _) in &patterns {
        if !names.insert(name) {
            return Err(Error::DuplicatePattern(name.clone()));
        }
    }
    Ok(patterns)
}

fn compile(patterns: Vec<(String, String)>) -> Result<Vec<(String, Regex)>, Error> {
    patterns
        .into_iter()
        .map(|(key, val)| {
            Regex::new(&val)
                .map(|regex| (key, regex))
                .map_err(Error::InvalidRegex)
        })
        .collect()
}

fn force_start_anchor(pattern: &str) -> String {
    let mut prev_char: Option<char> = None;

    let anchor_indices: HashSet<usize> = pattern
        .char_indices()
        .filter(|(_, c)| {
            let prev = prev_char.unwrap_or('\0');
            prev_char = Some(*c);
            *c == '^' && !matches!(prev, '[' | '\\')
        })
        .map(|(i, _)| i)
        .collect();

    format!(
        "^({})",
        pattern
            .chars()
            .enumerate()
            .filter_map(|(i, char)| (!anchor_indices.contains(&i)).then_some(char))
            .collect::<String>()
    )
}

fn adjust(patterns: Vec<(String, String)>) -> Vec<(String, String)> {
    patterns
        .into_iter()
        .map(|(name, pattern)| (name, force_start_anchor(&pattern)))
        .collect()
}
