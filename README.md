# crossandra-rs
`crossandra-rs` is a work-in-progress ⚠️, straightforward tokenization library for seamless text
processing. A simplified Rust implementation of the Python [Crossandra] library.

## Usage
Add this to your `Cargo.toml`:
```toml
[dependencies]
crossandra = "0.0.1"
```

Import and use like this:
```rust
use crossandra::{Tokenizer, common};

fn main() {
    let word_finder = Tokenizer::default()
        .with_patterns(vec![common::WORD.clone()])
        .expect("built-in pattern should be safe");

    let text = "Hello, world!";

    for token in word_finder.tokenize(text).flatten() {
        println!("{:?}", token);
    }
    // Token { name: "word", value: "Hello", position: 0}
    // Token { name: "word", value: "world", position: 7}
}
```
---
### Documentation
The documentation is available at [docs.rs/crossandra][docs].

### Acknowledgements
Huge thanks to [@Maneren][Maneren] for his invaluable guidance in developing
this library 🫶

### License
`crossandra-rs` is licensed under the [MIT License].  
© [trag1c], 2024

[Crossandra]: https://github.com/trag1c/crossandra
[docs]: https://docs.rs/crossandra/
[Maneren]: https://github.com/Maneren
[MIT License]: https://opensource.org/license/mit/
[trag1c]: https://github.com/trag1c
