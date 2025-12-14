# `--C`

Goals of this programming language:
1. Syntax supported is a subset of C
   1. Any additional syntax that isn't supported by the C spec should
        come with support for transpilation to C
   2. An exception to this is support for infinite length integers
2. It can compile to x86-64 assembly
3. It can compile down to cellular automata
    1. There should be a fixed position or range of positions from
       which it can be determined whether the program has halted
4. True support for infinite length integers
   1. This is as opposed to arbitrary length integers with a length that is
      bounded by the largest int datatype
   2. More specifically the infinite length integer should theoretically be able to 
      actually grow to infinity assuming a machine with an infinite address space 
      and infinite register size, but where the data size at each address 
      is finite still


# Setup
Rust is required to build the compiler.  
Pull the C compiler tests submodule with `git submodule update --init --recursive`
1. `cargo build --release`
2. `./target/release/ca-compiler <YOUR_C_FILE.c>`

For testing things related to the Potato CPU:
1. Create a python3.12 virtual environment - `python3.12 -m venv venv`
2. Activate the virtual environment - `source venv/bin/activate`
3. Install python dependencies - `pip install -r requirements.txt`
4. Generate pyO3 stubs - `cargo run --bin stub_gen`
5. build and install the potato CPU - `maturin develop --release`

## Examples

Test compilation on all test cases in chapter 2:
```bash
cargo build && \
writing-a-c-compiler-tests/test_compiler ./target/debug/ca-compiler --chapter 2
```

Test asm gen on all test cases in chapter 2:
```bash
cargo build && \
writing-a-c-compiler-tests/test_compiler ./target/debug/ca-compiler --chapter 2 --stage codegen
```

Run code generation for a single file:
```bash
cargo build && \
./target/debug/ca-compiler writing-a-c-compiler-tests/tests/chapter_3/valid/unop_parens.c --stage codegen
```

Supported stages:
- lex
- parse
- tacky
- codegen

## TODO
- implement tacky + code gen for relational operators
- support chapter 2 test suit when targeting Potato CPU
- implement assembly gen for relational operators
- implement multi-tape cellular automata codegen
- implement multi-tape cellular automata composition
- multi-tape to single-tape cellular automata compiler
- implement web automata visualizer tool
- implement TUI for automata visualization
- add annotated blocks for assembly generated

# DONE
- support chapter 1 test suite when targeting Potato CPU
- add precedence climbing for expressions
- implement tacky IR generation
- binary operators support
- unary operators support
- recursive descent parser
- implement assembly generation
