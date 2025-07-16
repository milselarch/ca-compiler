# ca-compiler

## `C compiler tests`
### `run lexer tests`
`cargo build && python ./writing-a-c-compiler-tests/test_compiler ./target/debug/R110 --chapter 1 --stage lex`  
examples for running lexer tests for specific test cases
1. `cargo build`  
`./target/debug/ca-compiler --lex ~/projects/ca-compiler/writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/end_before_expr.c`
2. `cargo build`  
`./target/debug/ca-compiler --lex ~/projects/ca-compiler/writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/invalid_function_name.c`
  
Running on windows: 
1. `cargo build`  
`./target/debug/ca-compiler.exe --lex ./writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/end_before_expr.c`
2. `cargo build`  
`./target/debug/ca-compiler.exe --lex ./writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/invalid_function_name.c`

