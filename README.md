# Monkey

[![Build Status](https://travis-ci.org/joecorcoran/monkey.svg?branch=master)](https://travis-ci.org/joecorcoran/monkey)

Implementation of the functional language Monkey from [Writing an Interpreter in Go](https://interpreterbook.com/), written in Rust.

Builds only on nightly, as the lexer relies on [the experimental unicode API](https://doc.rust-lang.org/std_unicode/str/trait.UnicodeStr.html).

```
git clone https://github.com/joecorcoran/monkey.git
cd monkey
rustup install nightly
rustup override set nightly
cargo run
```
