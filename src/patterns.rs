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

#[cfg(test)]
mod tests {
    use crate::{
        error::Error,
        patterns::{compile, force_start_anchor, validate},
    };

    #[test]
    fn validate_ok() {
        let patterns = vec![("foo".into(), String::new()), ("bar".into(), String::new())];
        assert!(validate(patterns).is_ok());
    }

    #[test]
    fn validate_empty_ok() {
        assert!(validate(Vec::new()).is_ok());
    }

    #[test]
    fn validate_err() {
        let patterns = vec![("foo".into(), String::new()), ("foo".into(), String::new())];
        let res = validate(patterns);
        assert!(matches!(res, Err(Error::DuplicatePattern(s)) if s == *"foo"));
    }

    #[test]
    fn validate_two_duplicate_keys_err() {
        let patterns = vec![
            ("foo".into(), String::new()),
            ("bar".into(), String::new()),
            ("bar".into(), String::new()),
            ("foo".into(), String::new()),
        ];
        let res = validate(patterns);
        assert!(matches!(res, Err(Error::DuplicatePattern(s)) if s == *"bar"));
    }

    #[test]
    fn compile_ok() {
        let patterns = vec![("foo".into(), String::new()), ("bar".into(), r"\d+".into())];
        assert!(compile(patterns).is_ok());
    }

    #[test]
    fn compile_err() {
        let patterns = vec![("foo".into(), String::new()), ("bar".into(), r"+".into())];
        assert!(matches!(compile(patterns), Err(Error::InvalidRegex(_))));
    }

    #[test]
    fn adjust() {
        let tests = [
            ("", "^()"),
            (r"\d+", r"^(\d+)"),
            (r"^\d+", r"^(\d+)"),
            (r"x|^y", r"^(x|y)"),
            (r"^x|^y", r"^(x|y)"),
            (r"^x|\^y", r"^(x|\^y)"),
            (r"ba[^rz]", r"^(ba[^rz])"),
            (r"^\^^[^]^", r"^(\^[^])"),
        ];
        for (inp, out) in tests {
            assert_eq!(force_start_anchor(inp), out);
        }
    }
}
