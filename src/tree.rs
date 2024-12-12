use rustc_hash::FxHashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Tree {
    Leaf(String),
    Node(FxHashMap<Option<char>, Tree>),
}

pub(crate) fn generate_tree(literals: &FxHashMap<&str, &str>) -> Tree {
    let mut sorted_items: Vec<_> = literals.iter().collect();
    sorted_items.sort_by_key(|(k, _)| std::cmp::Reverse(k.len()));

    let mut root = Tree::Node(FxHashMap::default());

    for (k, &v) in sorted_items {
        let mut current = &mut root;

        // iterate over the characters in the key
        let mut chars = k.chars().peekable();
        while let Some(c) = chars.next() {
            let Tree::Node(ref mut map) = current else {
                continue;
            };

            // if there is a character after the current character
            if chars.peek().is_some() {
                // move down the tree
                current = map
                    .entry(Some(c))
                    .or_insert(Tree::Node(FxHashMap::default()));
            } else {
                // else we reached the end and insert the value at the current position
                map.entry(Some(c))
                    // if the current subtree is a node, insert the value as a subtree
                    .and_modify(|inner_tree| {
                        if let Tree::Node(node) = inner_tree {
                            node.insert(None, Tree::Leaf(v.to_string()));
                        }
                    })
                    // if the current subtree is a node, insert the value as a leaf
                    .or_insert(Tree::Leaf(v.to_string()));

                break; // needed to satisfy the borrow checker
            }
        }
    }

    root
}

#[cfg(test)]
mod tests {
    use rustc_hash::FxHashMap;

    use super::{
        generate_tree,
        Tree::{Leaf, Node},
    };

    macro_rules! hashmap {
        { $( $key:expr => $value:expr ),* $(,)? } => {{
            FxHashMap::from_iter([$( ($key, $value), )*])
        }};
    }

    #[test]
    fn empty_tree() {
        let tree = generate_tree(&hashmap! {});
        assert!(matches!(tree, Node(FxHashMap { .. })));
    }

    #[test]
    fn flat_tree() {
        let tree = generate_tree(&hashmap! {
            "+" => "add",
            "-" => "sub",
            "<" => "left",
            ">" => "right",
            "," => "read",
            "." => "write",
            "[" => "begin_loop",
            "]" => "end_loop",
        });

        let Node(tree) = tree else {
            panic!("tree was not a Node");
        };

        assert_eq!(
            tree,
            hashmap! {
                Some('+') => Leaf("add".into()),
                Some(']') => Leaf("end_loop".into()),
                Some('-') => Leaf("sub".into()),
                Some('[') => Leaf("begin_loop".into()),
                Some(',') => Leaf("read".into()),
                Some('.') => Leaf("write".into()),
                Some('>') => Leaf("right".into()),
                Some('<') => Leaf("left".into()),
            }
        );
    }

    #[test]
    fn basic_nested_tree() {
        let tree = generate_tree(&hashmap! {
            "ABC" => "abc",
            "ACB" => "acb",
            "BAC" => "bac",
            "BCA" => "bca",
            "CAB" => "cab",
            "CBA" => "cba",
        });

        assert_eq!(
            tree,
            Node(hashmap! {
                Some('A') => Node(hashmap! {
                    Some('B') => Node(hashmap! { Some('C') => Leaf("abc".into()) }),
                    Some('C') => Node(hashmap! { Some('B') => Leaf("acb".into()) })
                }),
                Some('B') => Node(hashmap! {
                    Some('A') => Node(hashmap! { Some('C') => Leaf("bac".into()) }),
                    Some('C') => Node(hashmap! { Some('A') => Leaf("bca".into()) })
                }),
                Some('C') => Node(hashmap! {
                    Some('A') => Node(hashmap! { Some('B') => Leaf("cab".into()) }),
                    Some('B') => Node(hashmap! { Some('A') => Leaf("cba".into()) })
                }),
            })
        );
    }

    #[test]
    fn break_path_nested_tree() {
        let tree = generate_tree(&hashmap! {
            "ABC" => "x",
            "A" => "y",
            "B" => "z",
        });

        assert_eq!(
            tree,
            Node(hashmap! {
                Some('A') => Node(hashmap! {
                    Some('B') => Node(hashmap! {
                        Some('C') => Leaf("x".into())
                    }),
                    None => Leaf("y".into())
                }),
                Some('B') => Leaf("z".into())
            })
        );
    }

    #[test]
    fn same_symbol_tree() {
        let tree = generate_tree(&hashmap! {
            "+" => "a",
            "++" => "b",
            "+++" => "c",
            "++++" => "d",
            "+++++" => "e",
            "++++++" => "f",
        });

        assert!(matches!(tree, Node { .. }));

        let expected_tree = Node(hashmap! {
            Some('+') => Node(hashmap! {
                None => Leaf("a".into()),
                Some('+') => Node(hashmap! {
                    None => Leaf("b".into()),
                    Some('+') => Node(hashmap! {
                        None => Leaf("c".into()),
                        Some('+') => Node(hashmap! {
                            None => Leaf("d".into()),
                            Some('+') => Node(hashmap! {
                                None => Leaf("e".into()),
                                Some('+') => Leaf("f".into())
                            })
                        })
                    })
                })
            })
        });

        assert_eq!(tree, expected_tree);
    }

    #[test]
    #[allow(clippy::too_many_lines)]
    fn samarium_tree() {
        let tree = generate_tree(&hashmap! {
            "+" => "ad",
            "&&" => "an",
            "@@@" => "ar",
            ":" => "as",
            "." => "at",
            "&" => "ba",
            "~" => "bn",
            "|" => "bo",
            "}" => "brace_c",
            "{" => "brace_o",
            "]" => "brack_c",
            "[" => "brack_o",
            "^" => "bx",
            "%" => "cas",
            "!!" => "cat",
            "@" => "cl",
            "@!" => "da",
            "<>" => "de",
            "--" => "di",
            ",," => "e",
            ";" => "end",
            "=>" => "ent",
            "#" => "enu",
            "::" => "eq",
            "=>!" => "ex",
            "&~~>" => "fi_a",
            "&%~>" => "fi_b_a",
            "<~%" => "fi_b_r",
            "<%>" => "fi_b_r_w",
            "%~>" => "fi_b_w",
            "?~>" => "fi_c",
            "&~>" => "fi_q_a",
            "&%>" => "fi_q_b_a",
            "<%" => "fi_q_b_r",
            "%>" => "fi_q_b_w",
            "<~" => "fi_q_r",
            "~>" => "fi_q_w",
            "<~>" => "fi_r_w",
            "<~~" => "fi_r",
            "~~>" => "fi_w",
            "..." => "fo",
            "<-" => "fr",
            "*" => "fu",
            ">:" => "ge",
            ">" => "gt",
            "##" => "h",
            "?" => "if",
            "<=" => "im",
            "->?" => "in",
            "'" => "ins",
            "<:" => "le",
            "<" => "lt",
            "---" => "mo",
            "++" => "mu",
            ":::" => "ne",
            "~~" => "no",
            "||" => "o",
            ")" => "p_c",
            "(" => "p_o",
            "!?" => "pa",
            "+++" => "po",
            "!" => "pr",
            "???" => "r",
            "," => "se",
            ",.," => "sle",
            ">>" => "s_c",
            "<<" => "s_o",
            "$" => "sp",
            "-" => "su",
            "}}" => "t_c",
            "{{" => "t_o",
            "!!!" => "th",
            "->" => "to",
            "??" => "tr",
            "?!" => "ty",
            "@@" => "u",
            ".." => "w",
            "^^" => "x",
            "**" => "y",
            "><" => "z",
        });

        assert!(matches!(tree, Node { .. }));

        let expected_tree = Node(hashmap! {
            Some('%') => Node(hashmap! {
                None => Leaf("cas".into()),
                Some('>') => Leaf("fi_q_b_w".into()),
                Some('~') => Node(hashmap! { Some('>') => Leaf("fi_b_w".into()) })
            }),
            Some('&') => Node(hashmap! {
                None => Leaf("ba".into()),
                Some('&') => Leaf("an".into()),
                Some('~') => Node(hashmap! {
                    Some('~') => Node(hashmap! { Some('>') => Leaf("fi_a".into()) }),
                    Some('>') => Leaf("fi_q_a".into())
                }),
                Some('%') => Node(hashmap! {
                    Some('>') => Leaf("fi_q_b_a".into()),
                    Some('~') => Node(hashmap! { Some('>') => Leaf("fi_b_a".into()) })
                })
            }),
            Some('-') => Node(hashmap! {
                None => Leaf("su".into()),
                Some('>') => Node(hashmap! {
                    Some('?') => Leaf("in".into()),
                    None => Leaf("to".into())
                }),
                Some('-') => Node(hashmap! {
                    Some('-') => Leaf("mo".into()),
                    None => Leaf("di".into())
                })
            }),
            Some('>') => Node(hashmap! {
                None => Leaf("gt".into()),
                Some(':') => Leaf("ge".into()),
                Some('>') => Leaf("s_c".into()),
                Some('<') => Leaf("z".into())
            }),
            Some('^') => Node(hashmap! {
                None => Leaf("bx".into()),
                Some('^') => Leaf("x".into())
            }),
            Some('$') => Leaf("sp".into()),
            Some('!') => Node(hashmap! {
                None => Leaf("pr".into()),
                Some('?') => Leaf("pa".into()),
                Some('!') => Node(hashmap! {
                    None => Leaf("cat".into()),
                    Some('!') => Leaf("th".into())
                })
            }),
            Some('|') => Node(hashmap! {
                None => Leaf("bo".into()),
                Some('|') => Leaf("o".into())
            }),
            Some('*') => Node(hashmap! {
                None => Leaf("fu".into()),
                Some('*') => Leaf("y".into())
            }),
            Some('(') => Leaf("p_o".into()),
            Some('<') => Node(hashmap! {
                None => Leaf("lt".into()),
                Some('-') => Leaf("fr".into()),
                Some('<') => Leaf("s_o".into()),
                Some(':') => Leaf("le".into()),
                Some('>') => Leaf("de".into()),
                Some('=') => Leaf("im".into()),
                Some('~') => Node(hashmap! {
                    None => Leaf("fi_q_r".into()),
                    Some('>') => Leaf("fi_r_w".into()),
                    Some('%') => Leaf("fi_b_r".into()),
                    Some('~') => Leaf("fi_r".into())
                }),
                Some('%') => Node(hashmap! {
                    None => Leaf("fi_q_b_r".into()),
                    Some('>') => Leaf("fi_b_r_w".into())
                })
            }),
            Some('@') => Node(hashmap! {
                None => Leaf("cl".into()),
                Some('!') => Leaf("da".into()),
                Some('@') => Node(hashmap! {
                    None => Leaf("u".into()),
                    Some('@') => Leaf("ar".into())
                })
            }),
            Some('=') => Node(hashmap! {
                Some('>') => Node(hashmap! {
                    None => Leaf("ent".into()),
                    Some('!') => Leaf("ex".into())
                })
            }),
            Some('?') => Node(hashmap! {
                None => Leaf("if".into()),
                Some('!') => Leaf("ty".into()),
                Some('~') => Node(hashmap! { Some('>') => Leaf("fi_c".into()) }),
                Some('?') => Node(hashmap! {
                    None => Leaf("tr".into()),
                    Some('?') => Leaf("r".into())
                })
            }),
            Some('~') => Node(hashmap! {
                None => Leaf("bn".into()),
                Some('>') => Leaf("fi_q_w".into()),
                Some('~') => Node(hashmap! {
                    None => Leaf("no".into()),
                    Some('>') => Leaf("fi_w".into())
                })
            }),
            Some('{') => Node(hashmap! {
                None => Leaf("brace_o".into()),
                Some('{') => Leaf("t_o".into())
            }),
            Some('}') => Node(hashmap! {
                None => Leaf("brace_c".into()),
                Some('}') => Leaf("t_c".into())
            }),
            Some('+') => Node(hashmap! {
                None => Leaf("ad".into()),
                Some('+') => Node(hashmap! {
                    None => Leaf("mu".into()),
                    Some('+') => Leaf("po".into())
                })
            }),
            Some(')') => Leaf("p_c".into()),
            Some(',') => Node(hashmap! {
                None => Leaf("se".into()),
                Some(',') => Leaf("e".into()),
                Some('.') => Node(hashmap! { Some(',') => Leaf("sle".into()) })
            }),
            Some('\'') => Leaf("ins".into()),
            Some(':') => Node(hashmap! {
                None => Leaf("as".into()),
                Some(':') => Node(hashmap! {
                    None => Leaf("eq".into()),
                    Some(':') => Leaf("ne".into())
                })
            }),
            Some('#') => Node(hashmap! {
                None => Leaf("enu".into()),
                Some('#') => Leaf("h".into())
            }),
            Some('[') => Leaf("brack_o".into()),
            Some(';') => Leaf("end".into()),
            Some(']') => Leaf("brack_c".into()),
            Some('.') => Node(hashmap! {
                None => Leaf("at".into()),
                Some('.') => Node(hashmap! {
                    None => Leaf("w".into()),
                    Some('.') => Leaf("fo".into())
                })
            })
        });

        assert_eq!(tree, expected_tree);
    }
}
