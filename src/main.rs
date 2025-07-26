use std::env;
use std::io::{self};
use std::path::Path;
use crate::lexer::lexer::lex_from_filepath;
use crate::parser::asm_symbols::AsmSymbol;
pub mod lexer;
pub mod parser;


fn print_usage(args: &Vec<String>) {
    eprintln!("Unknown / invalid args: {:?}", args);
    eprintln!("Usage: {} --lex <file_path>", args[0]);
    eprintln!("Usage: {} --parse <file_path>", args[0]);
    eprintln!("Usage: {} --codegen <file_path>", args[0]);
}

fn main() -> io::Result<()> {
    // Collect command line arguments
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        print_usage(&args);
        std::process::exit(1);
    }

    // println!("Arguments: {:?}", args);
    let first_arg = &args[1];
    let path = Path::new(first_arg);
    // println!("Checking if path exists: {}", path.display());
    if path.exists() {
        let source_filepath = first_arg;
        let output_path = path.with_extension("");
        let asm_gen_result = parser::parser::asm_gen_from_filepath(source_filepath, true);

        // Write the generated assembly code to the output file
        let asm_program = match asm_gen_result {
            Ok(program) => program,
            Err(err) => {
                eprintln!("Error generating assembly: {}", err);
                std::process::exit(1);
            }
        };

        let asm_code = asm_program.to_asm_code();
        // Write the assembly code to the output file
        std::fs::write(&output_path, asm_code).expect("Unable to write file");
        eprintln!("Output path: {}", output_path.display());
        std::process::exit(1);
    }

    // Check if the correct number of arguments is provided
    if args.len() != 3 {
        print_usage(&args);
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
        "--codegen" => {
            // Proceed with assembly generation
            let asm_gen_result = parser::parser::asm_gen_from_filepath(&args[2], true);
            if asm_gen_result.is_err() {
                eprintln!("Assembly Generation Error: {}", asm_gen_result.err().unwrap());
                std::process::exit(1);
            } else {
                println!("Assembly Generation successful!");
                std::process::exit(0);
            }
        },
        _ => {
            print_usage(&args);
            std::process::exit(1);
        }
    }
}
