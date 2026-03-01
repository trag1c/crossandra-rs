use fancy_regex::Regex;
use rustc_hash::FxHashSet;

use crate::error::Error;

pub(crate) fn prepare<'a>(patterns: &[(&'a str, &'a str)]) -> Result<Vec<(&'a str, Regex)>, Error> {
    compile(adjust(patterns))
}

fn compile(patterns: Vec<(&str, String)>) -> Result<Vec<(&str, Regex)>, Error> {
    patterns
        .into_iter()
        .map(|(key, val)| {
            Regex::new(&val)
                .map(|regex| (key, regex))
                .map_err(|e| Error::InvalidRegex(Box::new(e)))
        })
        .collect()
}

fn force_start_anchor(pattern: &str) -> String {
    let mut prev_char: Option<char> = None;

    let anchor_indices: FxHashSet<usize> = pattern
        .char_indices()
        .filter(|(_, c)| {
            let prev = prev_char.unwrap_or('\0');
            prev_char = Some(*c);
            *c == '^' && !matches!(prev, '[' | '\\')
        })
        .map(|(i, _)| i)
        .collect();

    format!(
        "^(?:{})",
        pattern
            .chars()
            .enumerate()
            .filter_map(|(i, char)| (!anchor_indices.contains(&i)).then_some(char))
            .collect::<String>()
    )
}

fn adjust<'a>(patterns: &[(&'a str, &'a str)]) -> Vec<(&'a str, String)> {
    patterns
        .iter()
        .map(|(name, pattern)| (*name, force_start_anchor(pattern)))
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::{
        error::Error,
        patterns::{compile, force_start_anchor, prepare},
    };

    #[test]
    fn compile_ok() {
        let patterns = vec![("foo", String::new()), ("bar", r"\d+".into())];
        assert!(compile(patterns).is_ok());
    }

    #[test]
    fn compile_err() {
        let patterns = vec![("foo", String::new()), ("bar", r"+".into())];
        assert!(matches!(compile(patterns), Err(Error::InvalidRegex(_))));
    }

    #[test]
    fn adjust() {
        let tests = [
            ("", "^(?:)"),
            (r"\d+", r"^(?:\d+)"),
            (r"^\d+", r"^(?:\d+)"),
            (r"x|^y", r"^(?:x|y)"),
            (r"^x|^y", r"^(?:x|y)"),
            (r"^x|\^y", r"^(?:x|\^y)"),
            (r"ba[^rz]", r"^(?:ba[^rz])"),
            (r"^\^^[^]^", r"^(?:\^[^])"),
        ];
        for (inp, out) in tests {
            assert_eq!(force_start_anchor(inp), out);
        }
    }

    #[test]
    fn prepare_ok() {
        let Ok(patterns) = prepare(&[("digit", "[0-9]".into())]) else {
            panic!("prepare returned an Err")
        };
        match &patterns[..] {
            [(name, pat)] => {
                assert_eq!(*name, "digit");
                assert_eq!(pat.as_str(), "^(?:[0-9])");
            }
            _ => panic!("prepare returned a vec of length != 1"),
        };
    }

    #[test]
    fn prepare_err() {
        assert!(prepare(&[("digit", "[0-9".into())]).is_err());
        assert!(prepare(&[("digit", "[0-9]".into()), ("digit", "[0-9]".into())]).is_ok());
    }
}
