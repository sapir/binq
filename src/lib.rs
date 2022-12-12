mod array_try_map;
mod convert_ir;
mod database;

use pyo3::prelude::*;

use self::{convert_ir::IrConverter, database::Database};

#[pyclass(name = "Database")]
struct PyDatabase(Database);

#[pymethods]
impl PyDatabase {
    #[new]
    fn new() -> Self {
        Self(Database::default())
    }

    fn add_block(&mut self, py: Python, irsb: &PyAny) -> PyResult<()> {
        // TODO: cache the converter
        let pyvex = py.import("pyvex")?;
        let converter = IrConverter::new(pyvex)?;

        let block = converter.convert_block(irsb)?;
        self.0.add_block(block);

        Ok(())
    }
}

#[pymodule]
fn binq(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<PyDatabase>()?;
    Ok(())
}
