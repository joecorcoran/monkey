#/bin/sh

set -ex

cargo build -p monkey-web --target wasm32-unknown-unknown --release
wasm-bindgen ../target/wasm32-unknown-unknown/release/monkey_web.wasm --out-dir ./dist/ --no-typescript
