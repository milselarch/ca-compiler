# ca-compiler

### TODO:
- trace tokens to source code positions 
- trace ASM lines to AST nodes
- generate turing machine tape from code
- implement arbitrary (specified) length integer type support
- implement turing machine tape to CA conversion

# `C compiler tests`

## `CHAPTER 2`

Test lexer on all test cases in chapter 2:
```bash
cargo build --release && \
writing-a-c-compiler-tests/test_compiler ./target/debug/ca-compiler --chapter 2 --stage lex
```
  
Lexing a specific test case:  
```bash
cargo build && ./target/debug/ca-compiler --lex \
/home/milselarch/projects/ca-compiler/writing-a-c-compiler-tests/tests/chapter_2/valid/redundant_parens.c
```

## `CHAPTER 1`
### `run lexer tests`
`cargo build --release && python ./writing-a-c-compiler-tests/test_compiler ./target/release/R110 --chapter 1 --stage lex`  
examples for running lexer tests for specific test cases
1. `cargo build`  
`./target/debug/ca-compiler --lex ~/projects/ca-compiler/writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/end_before_expr.c`
2. `cargo build`  
`./target/debug/ca-compiler --lex ~/projects/ca-compiler/writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/invalid_function_name.c`

Test lexing on all test cases in chapter 1:  
```bash
writing-a-c-compiler-tests/test_compiler ./target/debug/ca-compiler --chapter 1 --stage lex
```

Running on windows: 
1. `cargo build`  
`./target/debug/ca-compiler.exe --lex ./writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/end_before_expr.c`
2. `cargo build`  
`./target/debug/ca-compiler.exe --lex ./writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/invalid_function_name.c`

### `run parser tests`

`./target/debug/ca-compiler --parse 
~/projects/ca-compiler/writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/end_before_expr.c`  
`./target/debug/ca-compiler --parse 
~/projects/ca-compiler/writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/invalid_function_name.c`

Test parsing on all test cases in chapter 1:
```bash
writing-a-c-compiler-tests/test_compiler ./target/debug/ca-compiler --chapter 1 --stage parse
```

### `run asm generation tests`

Test asm generation on all test cases in chapter 1:
```bash
writing-a-c-compiler-tests/test_compiler ./target/debug/ca-compiler --chapter 1 --stage codegen
```

### `chapter 1 tests`

Test asm generation on all test cases in chapter 1:
```bash
writing-a-c-compiler-tests/test_compiler ./target/release/ca-compiler --chapter 1
```


WE ARE NOW AT PAGE 17  
