use std::env;
use std::io::{self};
use crate::lexer::lexer::lex_from_filepath;

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

    let subcommand = &args[1][..];

    match subcommand {
        "--lex" => {
            let lex_result = lex_from_filepath(&args[2], true);
            if lex_result.is_err() {
                eprintln!("Error: {:?}", lex_result.err().unwrap());
                std::process::exit(1);
            } else {
                println!("Lex successful!");
                std::process::exit(0);
            }
        },
        "--parse" => {
            // Proceed with parsing
            // Get the file path from the arguments
            let parse_result = parser::parser::parse_from_filepath(&args[2], true);
            if parse_result.is_err() {
                eprintln!("Parse Error: {}", parse_result.err().unwrap());
                std::process::exit(1);
            } else {
                println!("Parse successful!");
                std::process::exit(0);
            }
        },
        _ => {
            eprintln!("Usage: {} --lex <file_path>", args[0]);
            eprintln!("Usage: {} --parse <file_path>", args[0]);
            std::process::exit(1);
        }
    }
}
