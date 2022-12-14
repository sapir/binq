// TODO: remove
#![allow(dead_code)]

mod analysis;
mod database;
mod ir;
mod lifting;
mod utils;

use pyo3::prelude::*;

use self::{database::Database, ir::Addr64};

#[pyclass(name = "Database")]
struct PyDatabase(Database);

#[pymethods]
impl PyDatabase {
    #[new]
    fn new() -> Self {
        Self(Database::default())
    }

    #[args(start_addr = "None")]
    fn add_func(&mut self, base_addr: Addr64, buf: &[u8], start_addr: Option<u64>) -> PyResult<()> {
        self.0
            .add_func(base_addr, buf, start_addr.unwrap_or(base_addr))
            .map_err(PyErr::from)
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
