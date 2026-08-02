# `C compiler tests`

## `CHAPTER 3`

Test parser on all test cases in chapter 3 (windows):
```bash
cargo build; 
python writing-a-c-compiler-tests/test_compiler ./target/debug/ca_compiler --chapter 3 --stage parse
```

Test lexer on all test cases in chapter 3 (windows):
```bash
cargo build; 
python writing-a-c-compiler-tests/test_compiler ./target/debug/ca_compiler --chapter 3 --stage lex
```


## `CHAPTER 2`

Test compilation on all test cases in chapter 2:
```bash
cargo build && \
writing-a-c-compiler-tests/test_compiler ./target/debug/ca_compiler --chapter 2
```

Test asm gen on all test cases in chapter 2:
```bash
cargo build && \
writing-a-c-compiler-tests/test_compiler ./target/debug/ca_compiler --chapter 2 --stage codegen
```  

Test tacky on all test cases in chapter 2:
```bash
cargo build && \
writing-a-c-compiler-tests/test_compiler ./target/debug/ca_compiler --chapter 2 --stage tacky
```  

Test lexer on all test cases in chapter 2:
```bash
cargo build && \
writing-a-c-compiler-tests/test_compiler ./target/debug/ca_compiler --chapter 2 --stage lex
```
Test lexer on all test cases in chapter 2 (windows):
```bash
cargo build; 
python writing-a-c-compiler-tests/test_compiler ./target/debug/ca_compiler --chapter 2 --stage lex
```

Lexing a specific test case:
```bash
cargo build && ./target/debug/ca_compiler --lex \
/home/milselarch/projects/ca_compiler/writing-a-c-compiler-tests/tests/chapter_2/valid/redundant_parens.c
```  
Lexing a specific test case (windows):
```bash
cargo build; ./target/debug/ca_compiler --lex \
./target/debug/ca_compiler.exe --lex ./writing-a-c-compiler-tests/tests/chapter_2/valid/parens_2.c
```  


## `CHAPTER 1`
### `run lexer tests`
`cargo build --release && python ./writing-a-c-compiler-tests/test_compiler ./target/release/R110 --chapter 1 --stage lex`  
examples for running lexer tests for specific test cases
1. `cargo build`  
   `./target/debug/ca_compiler --lex ~/projects/ca_compiler/writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/end_before_expr.c`
2. `cargo build`  
   `./target/debug/ca_compiler --lex ~/projects/ca_compiler/writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/invalid_function_name.c`

Test lexing on all test cases in chapter 1:
```bash
writing-a-c-compiler-tests/test_compiler ./target/debug/ca_compiler --chapter 1 --stage lex
```

Running on windows:
1. `cargo build`  
   `./target/debug/ca_compiler.exe --lex ./writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/end_before_expr.c`
2. `cargo build`  
   `./target/debug/ca_compiler.exe --lex ./writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/invalid_function_name.c`

### `run parser tests`

`./target/debug/ca_compiler --parse 
~/projects/ca_compiler/writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/end_before_expr.c`  
`./target/debug/ca_compiler --parse 
~/projects/ca_compiler/writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/invalid_function_name.c`

Test parsing on all test cases in chapter 1:
```bash
writing-a-c-compiler-tests/test_compiler ./target/debug/ca_compiler --chapter 1 --stage parse
```

### `run asm generation tests`

Test asm generation on all test cases in chapter 1:
```bash
writing-a-c-compiler-tests/test_compiler ./target/debug/ca_compiler --chapter 1 --stage codegen
```

### `chapter 1 tests`

Test asm generation on all test cases in chapter 1:
```bash
writing-a-c-compiler-tests/test_compiler ./target/release/ca_compiler --chapter 1
```
