# Rusty Idris

`rusty-idris` is a Rust backend for Idris 2, which aims for good interoperability with the Rust's ecosystem and complete support for all Idris 2 features. `rusty-idris` is in its early stages, please feel free to ask anything you are interested in.

## Quickstart

```shell
# Make sure you also installed the Idris 2 API.
$ idris2 --build rusty-idris.ipkg
$ ./build/exec/rusty-idris --cg rust ./tests/hello.idr -o ./tests/hello.rs
# Make sure you installed the Rust compiler.
$ rustc ./tests/hello.rs -o ./tests/hello && ./tests/hello
```
