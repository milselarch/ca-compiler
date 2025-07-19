# ca-compiler

## `C compiler tests`
### `run lexer tests`
`cargo build && python ./writing-a-c-compiler-tests/test_compiler ./target/debug/R110 --chapter 1 --stage lex`  
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

WE ARE NOW AT PAGE 17  
