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
