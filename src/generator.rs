use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use crate::{asm_gen, AssembleAndLinkError};
use crate::asm_gen::asm_symbols::AsmSymbol;

pub fn assemble_and_link(
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

pub fn compile_from_filepath(
    path: &std::path::Path
) {
    let source_filepath = path.to_str().unwrap();
    let asm_output_path = path.with_extension("s");
    let exec_output_path = path.with_extension("");
    let asm_gen_result =
        asm_gen::asm_symbols::asm_gen_from_filepath(source_filepath, true);

    let asm_program = match asm_gen_result {
        Ok(program) => program,
        Err(err) => {
            eprintln!("Error generating assembly: {}", err);
            std::process::exit(1);
        }
    };

    let asm_code = asm_program.to_asm_code();
    println!("\nGenerated assembly code:");
    println!("---------------------------------");
    println!("{}", asm_code);

    let file_res = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(&asm_output_path);

    let mut file = match file_res {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening output file: {}", err);
            std::process::exit(1);
        }
    };

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
