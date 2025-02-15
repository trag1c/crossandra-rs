use rustc_hash::FxHashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Tree<'a> {
    Leaf(&'a str),
    Node(FxHashMap<Option<char>, Tree<'a>>),
}

pub(crate) fn generate_tree<'a>(literals: &FxHashMap<&'a str, &'a str>) -> Tree<'a> {
    let mut sorted_items: Vec<_> = literals.iter().collect();
    sorted_items.sort_by_key(|(k, _)| std::cmp::Reverse(k.len()));

    let mut root: Tree<'a> = Tree::Node(FxHashMap::default());

    for (k, v) in sorted_items {
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
                            node.insert(None, Tree::Leaf(v));
                        }
                    })
                    // if the current subtree is a node, insert the value as a leaf
                    .or_insert(Tree::Leaf(v));

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
                Some('+') => Leaf("add"),
                Some(']') => Leaf("end_loop"),
                Some('-') => Leaf("sub"),
                Some('[') => Leaf("begin_loop"),
                Some(',') => Leaf("read"),
                Some('.') => Leaf("write"),
                Some('>') => Leaf("right"),
                Some('<') => Leaf("left"),
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
                    Some('B') => Node(hashmap! { Some('C') => Leaf("abc") }),
                    Some('C') => Node(hashmap! { Some('B') => Leaf("acb") })
                }),
                Some('B') => Node(hashmap! {
                    Some('A') => Node(hashmap! { Some('C') => Leaf("bac") }),
                    Some('C') => Node(hashmap! { Some('A') => Leaf("bca") })
                }),
                Some('C') => Node(hashmap! {
                    Some('A') => Node(hashmap! { Some('B') => Leaf("cab") }),
                    Some('B') => Node(hashmap! { Some('A') => Leaf("cba") })
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
                        Some('C') => Leaf("x")
                    }),
                    None => Leaf("y")
                }),
                Some('B') => Leaf("z")
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
                None => Leaf("a"),
                Some('+') => Node(hashmap! {
                    None => Leaf("b"),
                    Some('+') => Node(hashmap! {
                        None => Leaf("c"),
                        Some('+') => Node(hashmap! {
                            None => Leaf("d"),
                            Some('+') => Node(hashmap! {
                                None => Leaf("e"),
                                Some('+') => Leaf("f")
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
                None => Leaf("cas"),
                Some('>') => Leaf("fi_q_b_w"),
                Some('~') => Node(hashmap! { Some('>') => Leaf("fi_b_w") })
            }),
            Some('&') => Node(hashmap! {
                None => Leaf("ba"),
                Some('&') => Leaf("an"),
                Some('~') => Node(hashmap! {
                    Some('~') => Node(hashmap! { Some('>') => Leaf("fi_a") }),
                    Some('>') => Leaf("fi_q_a")
                }),
                Some('%') => Node(hashmap! {
                    Some('>') => Leaf("fi_q_b_a"),
                    Some('~') => Node(hashmap! { Some('>') => Leaf("fi_b_a") })
                })
            }),
            Some('-') => Node(hashmap! {
                None => Leaf("su"),
                Some('>') => Node(hashmap! {
                    Some('?') => Leaf("in"),
                    None => Leaf("to")
                }),
                Some('-') => Node(hashmap! {
                    Some('-') => Leaf("mo"),
                    None => Leaf("di")
                })
            }),
            Some('>') => Node(hashmap! {
                None => Leaf("gt"),
                Some(':') => Leaf("ge"),
                Some('>') => Leaf("s_c"),
                Some('<') => Leaf("z")
            }),
            Some('^') => Node(hashmap! {
                None => Leaf("bx"),
                Some('^') => Leaf("x")
            }),
            Some('$') => Leaf("sp"),
            Some('!') => Node(hashmap! {
                None => Leaf("pr"),
                Some('?') => Leaf("pa"),
                Some('!') => Node(hashmap! {
                    None => Leaf("cat"),
                    Some('!') => Leaf("th")
                })
            }),
            Some('|') => Node(hashmap! {
                None => Leaf("bo"),
                Some('|') => Leaf("o")
            }),
            Some('*') => Node(hashmap! {
                None => Leaf("fu"),
                Some('*') => Leaf("y")
            }),
            Some('(') => Leaf("p_o"),
            Some('<') => Node(hashmap! {
                None => Leaf("lt"),
                Some('-') => Leaf("fr"),
                Some('<') => Leaf("s_o"),
                Some(':') => Leaf("le"),
                Some('>') => Leaf("de"),
                Some('=') => Leaf("im"),
                Some('~') => Node(hashmap! {
                    None => Leaf("fi_q_r"),
                    Some('>') => Leaf("fi_r_w"),
                    Some('%') => Leaf("fi_b_r"),
                    Some('~') => Leaf("fi_r")
                }),
                Some('%') => Node(hashmap! {
                    None => Leaf("fi_q_b_r"),
                    Some('>') => Leaf("fi_b_r_w")
                })
            }),
            Some('@') => Node(hashmap! {
                None => Leaf("cl"),
                Some('!') => Leaf("da"),
                Some('@') => Node(hashmap! {
                    None => Leaf("u"),
                    Some('@') => Leaf("ar")
                })
            }),
            Some('=') => Node(hashmap! {
                Some('>') => Node(hashmap! {
                    None => Leaf("ent"),
                    Some('!') => Leaf("ex")
                })
            }),
            Some('?') => Node(hashmap! {
                None => Leaf("if"),
                Some('!') => Leaf("ty"),
                Some('~') => Node(hashmap! { Some('>') => Leaf("fi_c") }),
                Some('?') => Node(hashmap! {
                    None => Leaf("tr"),
                    Some('?') => Leaf("r")
                })
            }),
            Some('~') => Node(hashmap! {
                None => Leaf("bn"),
                Some('>') => Leaf("fi_q_w"),
                Some('~') => Node(hashmap! {
                    None => Leaf("no"),
                    Some('>') => Leaf("fi_w")
                })
            }),
            Some('{') => Node(hashmap! {
                None => Leaf("brace_o"),
                Some('{') => Leaf("t_o")
            }),
            Some('}') => Node(hashmap! {
                None => Leaf("brace_c"),
                Some('}') => Leaf("t_c")
            }),
            Some('+') => Node(hashmap! {
                None => Leaf("ad"),
                Some('+') => Node(hashmap! {
                    None => Leaf("mu"),
                    Some('+') => Leaf("po")
                })
            }),
            Some(')') => Leaf("p_c"),
            Some(',') => Node(hashmap! {
                None => Leaf("se"),
                Some(',') => Leaf("e"),
                Some('.') => Node(hashmap! { Some(',') => Leaf("sle") })
            }),
            Some('\'') => Leaf("ins"),
            Some(':') => Node(hashmap! {
                None => Leaf("as"),
                Some(':') => Node(hashmap! {
                    None => Leaf("eq"),
                    Some(':') => Leaf("ne")
                })
            }),
            Some('#') => Node(hashmap! {
                None => Leaf("enu"),
                Some('#') => Leaf("h")
            }),
            Some('[') => Leaf("brack_o"),
            Some(';') => Leaf("end"),
            Some(']') => Leaf("brack_c"),
            Some('.') => Node(hashmap! {
                None => Leaf("at"),
                Some('.') => Node(hashmap! {
                    None => Leaf("w"),
                    Some('.') => Leaf("fo")
                })
            })
        });

        assert_eq!(tree, expected_tree);
    }
}
