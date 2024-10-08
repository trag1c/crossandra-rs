use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Tree {
    Leaf(String),
    Node(HashMap<Option<char>, Tree>),
}

pub(crate) fn generate_tree(literals: &HashMap<&str, &str>) -> Tree {
    let mut sorted_items: Vec<_> = literals.iter().collect();
    sorted_items.sort_by(|(k1, _), (k2, _)| k2.len().cmp(&k1.len()));

    let mut root = Tree::Node(HashMap::new());

    for (k, v) in sorted_items {
        let mut current = &mut root;

        for c in k[..k.len() - 1].chars() {
            if let Tree::Node(ref mut map) = current {
                current = map.entry(Some(c)).or_insert(Tree::Node(HashMap::new()));
            }
        }

        if let Tree::Node(map) = current {
            let last_char = k.chars().last().expect("Key should not be empty");
            let token_name = Tree::Leaf((*v).to_string());
            match map.get_mut(&Some(last_char)) {
                Some(inner_tree) => {
                    if let Tree::Node(node) = inner_tree {
                        node.insert(None, token_name);
                    }
                }
                None => {
                    map.insert(Some(last_char), token_name);
                }
            };
        }
    }
    root
}
