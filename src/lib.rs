use pyo3::prelude::*;

use crate::potato_cpu::py_potato_cpu_tester::PyPotatoCPUTester;
use crate::automata::py_terms::{A, PyProduct, PyExpression};
use crate::automata::py_terms_multitape::{PyMultiTapeExpression, PyMultiTapeProduct, D};

pub mod lexer;
pub mod potato_cpu;
pub mod parser;
pub mod tacky;
pub mod asm_gen;
pub mod automata;
pub mod constants;

/// Formats the sum of two numbers as string.
#[pyfunction]
fn sum_as_string(a: usize, b: usize) -> PyResult<String> {
    Ok((a + b).to_string())
}

#[pymodule]
fn py_ca_compiler(module: &Bound<'_, PyModule>) -> PyResult<()> {
    module.add_function(wrap_pyfunction!(sum_as_string, module)?)?;
    module.add_class::<PyPotatoCPUTester>()?;

    module.add_class::<A>()?;
    module.add_class::<PyProduct>()?;
    module.add_class::<PyExpression>()?;
    
    module.add_class::<D>()?;
    module.add_class::<PyMultiTapeProduct>()?;
    module.add_class::<PyMultiTapeExpression>()?;
    Ok(())
}
