use std::env;
use std::fmt::Debug;
use std::fs::OpenOptions;
use std::io::{self, Write};
use std::os::unix::fs::OpenOptionsExt;
use std::path::Path;
use std::process::Command;
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

fn assemble_and_link(
    asm_path: &Path, exe_path: &Path
) -> Result<(), AssembleAndLinkError> {
    let status_res = Command::new("gcc")
        .arg("-o")
        .arg(exe_path)
        .arg(asm_path)
        .status();

    let status = match status_res {
        Ok(status) => status,
        Err(err) => return Err(AssembleAndLinkError::IoError(err)),
    };

    if !status.success() {
        let error_message = format!(
            "GCC failed with status: {}", status
        );
        return Err(AssembleAndLinkError::GccError(error_message));
    }
    Ok(())
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
        let asm_output_path = path.with_extension("s");
        let exec_output_path = path.with_extension("");
        let asm_gen_result = parser::parser::asm_gen_from_filepath(source_filepath, true);

        let asm_program = match asm_gen_result {
            Ok(program) => program,
            Err(err) => {
                eprintln!("Error generating assembly: {}", err);
                std::process::exit(1);
            }
        };

        let asm_code = asm_program.to_asm_code();
        println!("Generated assembly code:\n{}", asm_code);

        let mut file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .mode(0o755) // set permissions
            .open(&asm_output_path)?;

        // Write the generated assembly code
        let write_result = file.write_all(asm_code.as_bytes());
        if write_result.is_err() {
            eprintln!("Error writing to output file: {}", write_result.err().unwrap());
            std::process::exit(1);
        }
        // create executable from assembly
        // TODO: is there no way to pass the compiler tests
        //   by executing the assembly file directly?
        let assemble_result = assemble_and_link(&asm_output_path, &exec_output_path);
        if assemble_result.is_err() {
            eprintln!("Error assembling and linking: {:?}", assemble_result.err().unwrap());
            std::process::exit(1);
        } else {
            println!("Executable created at: {}", exec_output_path.display());
        }

        std::process::exit(0);
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
