use std::hint::black_box;

use criterion::{criterion_group, criterion_main, Criterion};
use crossandra::{common, Tokenizer};

pub fn samarium(c: &mut Criterion) {
    let literals = [
        ("ad", "+"),
        ("an", "&&"),
        ("ar", "@@@"),
        ("as", ":"),
        ("at", "."),
        ("ba", "&"),
        ("bn", "~"),
        ("bo", "|"),
        ("brace_c", "}"),
        ("brace_o", "{"),
        ("brack_c", "]"),
        ("brack_o", "["),
        ("bx", "^"),
        ("cas", "%"),
        ("cat", "!!"),
        ("cl", "@"),
        ("da", "@!"),
        ("de", "<>"),
        ("di", "--"),
        ("e", ",,"),
        ("end", ";"),
        ("ent", "=>"),
        ("enu", "#"),
        ("eq", "::"),
        ("ex", "=>!"),
        ("fi_a", "&~~>"),
        ("fi_b_a", "&%~>"),
        ("fi_b_r", "<~%"),
        ("fi_b_r_w", "<%>"),
        ("fi_b_w", "%~>"),
        ("fi_c", "?~>"),
        ("fi_q_a", "&~>"),
        ("fi_q_b_a", "&%>"),
        ("fi_q_b_r", "<%"),
        ("fi_q_b_w", "%>"),
        ("fi_q_r", "<~"),
        ("fi_q_w", "~>"),
        ("fi_r_w", "<~>"),
        ("fi_r", "<~~"),
        ("fi_w", "~~>"),
        ("fo", "..."),
        ("fr", "<-"),
        ("fu", "*"),
        ("ge", ">:"),
        ("gt", ">"),
        ("h", "##"),
        ("if", "?"),
        ("im", "<="),
        ("in", "->?"),
        ("ins", "'"),
        ("le", "<:"),
        ("lt", "<"),
        ("mo", "---"),
        ("mu", "++"),
        ("ne", ":::"),
        ("no", "~~"),
        ("o", "||"),
        ("p_c", ")"),
        ("p_o", "("),
        ("pa", "!?"),
        ("po", "+++"),
        ("pr", "!"),
        ("r", "???"),
        ("se", ","),
        ("sle", ",.,"),
        ("s_c", ">>"),
        ("s_o", "<<"),
        ("sp", "$"),
        ("su", "-"),
        ("t_c", "}}"),
        ("t_o", "{{"),
        ("th", "!!!"),
        ("to", "->"),
        ("tr", "??"),
        ("ty", "?!"),
        ("u", "@@"),
        ("w", ".."),
        ("x", "^^"),
        ("y", "**"),
        ("z", "><"),
    ];
    let patterns = [
        common::DOUBLE_QUOTED_STRING.clone(),
        ("number".into(), r"[\\/]+`?[\\/]*|`[\\/]*".into()),
        ("block_comment".into(), r"==<.*>==".into()),
        ("line_comment".into(), r"==[^\n]*".into()),
        ("variable".into(), r"\w+".into()),
    ];
    let tok = Tokenizer::default()
        .with_literals(&literals)
        .unwrap()
        .with_patterns(patterns.into())
        .unwrap()
        .with_ignore_whitespace(true);

    // one module from the samarium std
    let datetime = include_str!("assets/samarium/datetime.sm");

    c.bench_function("datetime.sm", |b| {
        b.iter(|| {
            black_box(
                tok.tokenize(black_box(datetime))
                    .collect::<Result<Vec<_>, _>>(),
            )
        })
    });

    // whole samarium std concatenated to one file
    let mut long_group = c.benchmark_group("samarium long");
    long_group.sample_size(10);
    let std = include_str!("assets/samarium/std.sm");

    long_group.bench_function("std.sm", |b| {
        b.iter(|| black_box(tok.tokenize(black_box(std)).collect::<Result<Vec<_>, _>>()))
    });
    let dtstripped = include_str!("assets/samarium/dtstripped.sm");

    long_group.bench_function("dtstripped.sm", |b| {
        b.iter(|| {
            black_box(
                tok.tokenize(black_box(dtstripped))
                    .collect::<Result<Vec<_>, _>>(),
            )
        })
    });
}

criterion_group!(benches, samarium);
criterion_main!(benches);
