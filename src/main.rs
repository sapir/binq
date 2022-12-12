mod array_try_map;
mod convert_ir;

use pyo3::prelude::*;

const TEST_ADDR: u64 = 0x1000;
const TEST_CODE: &[u8] = b"\x48\xc7\xc0\x64\x00\x00\x00\x04\xc8\x48\x89\xc3\xc3";

fn main() {
    pyo3::prepare_freethreaded_python();

    Python::with_gil(|py| -> PyResult<()> {
        let archinfo = py.import("archinfo")?;
        let amd64 = archinfo.getattr("ArchAMD64")?.call0()?;

        let pyvex = py.import("pyvex")?;
        let converter = convert_ir::IrConverter::new(pyvex)?;

        let irsb_class = pyvex.getattr("IRSB")?;
        let irsb = irsb_class.call1((TEST_CODE, TEST_ADDR, amd64))?;
        let irsb = converter.convert_block(irsb)?;

        dbg!(irsb);

        Ok(())
    })
    .unwrap();
}
