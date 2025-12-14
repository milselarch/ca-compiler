use std::env;
use std::fmt::Debug;
use std::io::{self};
use std::path::Path;
use crate::generator::compile_from_filepath;
use crate::lexer::lexer::lex_from_filepath;
use crate::tacky::tacky_symbols::PrintableTacky;

pub mod lexer;
pub mod parser;
pub mod constants;
pub mod generator;
pub mod tacky;
pub mod asm_gen;
pub mod potato_cpu;

fn print_usage(args: &Vec<String>) {
    eprintln!("Unknown / invalid args: {:?}", args);
    eprintln!("Usage: {} --lex <file_path>", args[0]);
    eprintln!("Usage: {} --parse <file_path>", args[0]);
    eprintln!("Usage: {} --codegen <file_path>", args[0]);
}

pub enum AssembleAndLinkError {
    IoError(std::io::Error),
    GccError(String),
}
impl Debug for AssembleAndLinkError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssembleAndLinkError::IoError(err) => write!(f, "IO Error: {}", err),
            AssembleAndLinkError::GccError(msg) => write!(f, "GCC Error: {}", msg),
        }
    }
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
        compile_from_filepath(path);
        return Ok(());
    }

    // Check if the correct number of arguments is provided
    if args.len() != 3 {
        print_usage(&args);
        std::process::exit(1);
    }

    let subcommand = &args[1][..];

    match subcommand {
        "--lex" => {
            let lex_result =
                lex_from_filepath(&args[2], true);
            if lex_result.is_err() {
                eprintln!("Error: {:?}", lex_result.err().unwrap());
                std::process::exit(1);
            } else {
                println!("Lex successful!");
                std::process::exit(0);
            }
        },
        "--parse" => {
            let parse_result =
                parser::parse::parse_from_filepath(&args[2], true);
            if parse_result.is_err() {
                eprintln!("Parse Error: {}", parse_result.err().unwrap());
                std::process::exit(1);
            } else {
                println!("Parse successful!");
                std::process::exit(0);
            }
        },
        "--tacky" => {
            let tacky_gen_result =
                tacky::tacky_symbols::tacky_gen_from_filepath(&args[2], true);
            if tacky_gen_result.is_err() {
                eprintln!("Tacky Generation Error: {}", tacky_gen_result.err().unwrap());
                std::process::exit(1);
            } else {
                println!("Tacky Generation successful!");
                let tacky_program = tacky_gen_result.unwrap();
                let tacky_code = tacky_program.print_tacky_code(0);
                println!("\nGenerated Tacky code:");
                println!("---------------------------------");
                println!("{}", tacky_code);
                std::process::exit(0);
            }
        },
        "--codegen" => {
            let asm_gen_result = 
                asm_gen::asm_symbols::asm_gen_from_filepath(&args[2], true);
            if asm_gen_result.is_err() {
                eprintln!("Assembly Generation Error: {:?}", asm_gen_result.err().unwrap());
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
