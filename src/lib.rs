// TODO: remove
#![allow(dead_code)]
#![allow(clippy::needless_late_init)]
#![allow(clippy::collapsible_match)]
#![allow(clippy::single_match)]
#![allow(clippy::collapsible_else_if)]

mod analysis;
mod database;
mod ir;
mod lifting;
mod query;
mod utils;

#[cfg(test)]
mod tests;

use database::StatementAddr;
use pyo3::{
    exceptions::PyValueError,
    prelude::*,
    types::{PyDict, PyLong},
};
use query::{search, CaptureValue, Expr, ExprMatch, ExprMatchFilter, Field};

use self::{
    database::Database,
    ir::{Addr64, CompareOpKind, Statement},
    query::ConditionExpr,
};

#[pyclass(name = "Expr")]
#[derive(Clone)]
struct PyExpr(Expr);

impl PyExpr {
    fn make_cmp(&self, kind: CompareOpKind, other: &Self) -> Self {
        Self(Expr::Condition(Box::new(ConditionExpr {
            kind,
            lhs: self.0.clone(),
            rhs: other.0.clone(),
        })))
    }
}

#[pymethods]
impl PyExpr {
    #[classattr]
    const ANY: Self = Self(Expr::Any);

    #[new]
    fn new(obj: &PyAny) -> PyResult<Self> {
        if obj.is_instance_of::<PyLong>()? {
            Ok(Self(Expr::Const(obj.extract()?)))
        } else {
            Err(PyValueError::new_err("Bad expression value"))
        }
    }

    fn named(&self, name: String) -> Self {
        Self(Expr::Named {
            inner: Box::new(self.0.clone()),
            name,
        })
    }

    fn deref(&self) -> Self {
        Self(Expr::Deref(Box::new(self.0.clone())))
    }

    fn eq(&self, other: &Self) -> Self {
        PyExpr::make_cmp(self, CompareOpKind::Equal, other)
    }

    fn ne(&self, other: &Self) -> Self {
        PyExpr::make_cmp(self, CompareOpKind::NotEqual, other)
    }

    fn ltu(&self, other: &Self) -> Self {
        PyExpr::make_cmp(self, CompareOpKind::LessThanUnsigned, other)
    }

    fn leu(&self, other: &Self) -> Self {
        PyExpr::make_cmp(self, CompareOpKind::LessThanOrEqualUnsigned, other)
    }

    fn lts(&self, other: &Self) -> Self {
        PyExpr::make_cmp(self, CompareOpKind::LessThanSigned, other)
    }

    fn les(&self, other: &Self) -> Self {
        PyExpr::make_cmp(self, CompareOpKind::LessThanOrEqualSigned, other)
    }

    fn __add__(&self, other: &Self) -> Self {
        let mut v = Vec::with_capacity(
            match &self.0 {
                Expr::Sum(v) => v.len(),
                _ => 1,
            } + match &other.0 {
                Expr::Sum(v) => v.len(),
                _ => 1,
            },
        );

        match &self.0 {
            Expr::Sum(v2) => {
                v.extend_from_slice(v2);
            }

            other => {
                v.push(other.clone());
            }
        }

        match &other.0 {
            Expr::Sum(v2) => {
                v.extend_from_slice(v2);
            }

            other => {
                v.push(other.clone());
            }
        }

        Self(Expr::Sum(v))
    }

    fn __mul__(&self, other: &Self) -> Self {
        let mut v = Vec::with_capacity(
            match &self.0 {
                Expr::Product(v) => v.len(),
                _ => 1,
            } + match &other.0 {
                Expr::Product(v) => v.len(),
                _ => 1,
            },
        );

        match &self.0 {
            Expr::Product(v2) => {
                v.extend_from_slice(v2);
            }

            other => {
                v.push(other.clone());
            }
        }

        match &other.0 {
            Expr::Product(v2) => {
                v.extend_from_slice(v2);
            }

            other => {
                v.push(other.clone());
            }
        }

        Self(Expr::Product(v))
    }
}

#[pyclass(name = "Database")]
struct PyDatabase(Database);

#[pymethods]
impl PyDatabase {
    #[new]
    fn new(arch: &str) -> PyResult<Self> {
        let arch = arch
            .parse()
            .map_err(|()| PyValueError::new_err("Bad value for 'arch'"))?;

        Ok(Self(Database::new(arch)))
    }

    fn add_buf(&mut self, addr: Addr64, buf: &[u8]) -> PyResult<()> {
        self.0.add_buf(addr, buf).map_err(PyErr::from)
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

    fn search<'py>(&mut self, py: Python<'py>, query: &PyDict) -> PyResult<Vec<&'py PyDict>> {
        let filters = {
            let mut filters = vec![];

            for (k, v) in query {
                let field: &str = k.extract()?;
                if field == "call_args" {
                    // Special case: The value is a list of arguments
                    let args: Vec<Option<PyExpr>> = v.extract()?;

                    filters.extend(args.into_iter().enumerate().filter_map(|(i, arg)| {
                        arg.map(|arg| ExprMatchFilter {
                            field: Field::CallArg(i),
                            expr: arg.0,
                        })
                    }));
                } else {
                    let field: Field = field
                        .parse()
                        .map_err(|()| PyValueError::new_err("Bad field name"))?;

                    let expr: PyExpr = v.extract()?;
                    let expr = expr.0;

                    filters.push(ExprMatchFilter { field, expr });
                }
            }

            filters
        };

        search(&mut self.0, &filters)
            .into_iter()
            .map(
                |ExprMatch {
                     match_addr,
                     captures,
                 }| {
                    let d = PyDict::new(py);
                    d.set_item("match_addr", (match_addr.asm_addr, match_addr.ir_index))?;
                    for (k, CaptureValue { expr, at }) in captures {
                        let v = match expr {
                            ir::Expr::Simple(ir::SimpleExpr::Const(x)) => x.to_object(py),
                            _ => expr.to_string().to_object(py),
                        };
                        let v_d = PyDict::new(py);
                        v_d.set_item("value", v)?;
                        v_d.set_item("at", (at.asm_addr, at.ir_index))?;
                        d.set_item(k, v_d)?;
                    }
                    Ok(d)
                },
            )
            .collect()
    }

    fn print_il(&mut self) {
        print_il(&mut self.0);
    }
}

fn print_il(database: &mut Database) {
    let mut stmts = database
        .world
        .query_mut::<(&StatementAddr, &Statement)>()
        .into_iter()
        .map(|(_entity, (addr, stmt))| (addr, stmt))
        .collect::<Vec<_>>();
    stmts.sort_by_key(|(addr, _stmt)| *addr);
    for (addr, stmt) in stmts {
        println!("{addr}: {stmt}");
    }
}

#[pymodule]
fn binq(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<PyDatabase>()?;
    m.add_class::<PyExpr>()?;
    Ok(())
}
