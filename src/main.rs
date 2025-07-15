use std::env;
use std::fs::File;
use std::io::{self, Read};

use lexer::lexer::Lexer;
pub mod lexer;
pub mod parser;

fn main() -> io::Result<()> {
    // Collect command line arguments
    let args: Vec<String> = env::args().collect();

    // Check if the correct number of arguments is provided
    if args.len() != 3 {
        eprintln!("Usage: {} --lex <file_path>", args[0]);
        std::process::exit(1);
    }
    // Check for the --lex flag
    if args[1] != "--lex" {
        eprintln!("Usage: {} --lex <file_path>", args[0]);
        std::process::exit(1);
    }

    // Get the file path from the arguments
    let file_path = &args[2];
    let mut file = File::open(file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    // Print the file contents
    println!("{}", contents);
    let lexer = Lexer::new();
    let tokens = lexer.tokenize(&contents);
    if tokens.is_err() {
        eprintln!("Error: {}", tokens.err().unwrap());
        std::process::exit(1);
    } else {
        std::process::exit(0);
    }
}