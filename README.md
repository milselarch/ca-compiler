# ca-compiler

## `C compiler tests`
### `run lexer tests`
`cargo build && python ./writing-a-c-compiler-tests/test_compiler ./target/debug/R110 --chapter 1 --stage lex`  
examples for running lexer tests for specific test cases
1. `cargo build && ./target/debug/R110 --lex ~/projects/ca-compiler/writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/end_before_expr.c`
2. `cargo build && ./target/debug/R110 --lex ~/projects/ca-compiler/writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/invalid_identifier_2.c`
