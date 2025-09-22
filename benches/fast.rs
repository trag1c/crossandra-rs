use std::hint::black_box;

use criterion::{criterion_group, criterion_main, Criterion};
use crossandra::Tokenizer;

pub fn brainfuck(c: &mut Criterion) {
    let tok = Tokenizer::default()
        .with_literals(&[
            ("add", "+"),
            ("sub", "-"),
            ("left", "<"),
            ("right", ">"),
            ("read", ","),
            ("write", "."),
            ("begin_loop", "["),
            ("end_loop", "]"),
        ])
        .unwrap();

    // 100 chars-long program that prints "Hello World!"
    let hello_world = include_str!("assets/brainfuck/hello_world.bf");

    c.bench_function("brainfuck hello_world", |b| {
        b.iter(|| {
            black_box(
                tok.tokenize(black_box(hello_world))
                    .collect::<Result<Vec<_>, _>>(),
            )
        })
    });

    // 12k chars-long program that visualizes the mandelbrot set
    let mandelbrot = include_str!("assets/brainfuck/mandelbrot.bf");

    c.bench_function("brainfuck mandelbrot", |b| {
        b.iter(|| {
            black_box(
                tok.tokenize(black_box(mandelbrot))
                    .collect::<Result<Vec<_>, _>>(),
            )
        })
    });

    // 50k chars-long program that solves the hanoi towers problem
    let hanoi = include_str!("assets/brainfuck/hanoi.bf");

    c.bench_function("brainfuck hanoi", |b| {
        b.iter(|| {
            black_box(
                tok.tokenize(black_box(hanoi))
                    .collect::<Result<Vec<_>, _>>(),
            )
        })
    });

    let mut huge_group = c.benchmark_group("brainfuck huge");
    huge_group.sample_size(30);
    // 1M chars-long file with random characters (mostly brainfuck with some comments)
    let huge = include_str!("assets/brainfuck/huge.bf");

    huge_group.bench_function("brainfuck huge", |b| {
        b.iter(|| {
            black_box(
                tok.tokenize(black_box(huge))
                    .filter_map(Result::ok)
                    .collect::<Vec<_>>(),
            )
        })
    });
}

criterion_group!(benches, brainfuck);
criterion_main!(benches);
