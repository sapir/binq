mod array_try_map;
mod convert_ir;

use pyo3::prelude::*;

use self::convert_ir::IrConverter;

#[pyfunction]
fn load(py: Python, irsb: &PyAny) -> PyResult<()> {
    // TODO: cache the converter
    let pyvex = py.import("pyvex")?;
    let converter = IrConverter::new(pyvex)?;

    let irsb = converter.convert_block(irsb)?;
    dbg!(irsb);

    Ok(())
}

#[pymodule]
fn binq(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(load, m)?)?;

    Ok(())
}
