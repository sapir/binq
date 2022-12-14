mod analysis;
mod database;
mod ir;
mod utils;

use pyo3::prelude::*;

use self::{database::Database, ir::IrConverter};

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

    fn analyze(&mut self) {
        analysis::analyze(&mut self.0);
    }
}

#[pymodule]
fn binq(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<PyDatabase>()?;
    Ok(())
}
