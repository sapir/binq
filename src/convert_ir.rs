mod ops;

use std::any::type_name;

use array_try_map::ArrayExt;
use phf::phf_map;
use pyo3::{
    exceptions::PyValueError,
    prelude::*,
    types::{PyFunction, PyString, PyType},
};

pub use self::ops::{Op1, Op2, Op3};

pub type Addr64 = u64;
pub type IRTemp = u32;

type PhfStringMap<T> = phf::Map<&'static str, T>;

fn py_string_to_enum<T: Copy>(map: &PhfStringMap<T>, string: &PyAny) -> PyResult<T> {
    string
        .cast_as::<PyString>()
        .ok()
        .and_then(|s| {
            let s = s.to_str().ok()?;
            map.get(s).copied()
        })
        .ok_or_else(|| {
            PyValueError::new_err(format!("Bad {} string {:?}", type_name::<T>(), string))
        })
}

#[derive(Clone, Copy, Debug)]
pub enum IRType {
    I1,
    I8,
    I16,
    I32,
    I64,
    I128,
    F16,
    F32,
    F64,
    D32,
    D64,
    D128,
    F128,
    V128,
    V256,
}

const IR_TYPES: PhfStringMap<IRType> = phf_map! {
    "Ity_I1" => IRType::I1,
    "Ity_I8" => IRType::I8,
    "Ity_I16" => IRType::I16,
    "Ity_I32" => IRType::I32,
    "Ity_I64" => IRType::I64,
    "Ity_I128" => IRType::I128,
    "Ity_F16" => IRType::F16,
    "Ity_F32" => IRType::F32,
    "Ity_F64" => IRType::F64,
    "Ity_D32" => IRType::D32,
    "Ity_D64" => IRType::D64,
    "Ity_D128" => IRType::D128,
    "Ity_F128" => IRType::F128,
    "Ity_V128" => IRType::V128,
    "Ity_V256" => IRType::V256,
};

impl<'source> FromPyObject<'source> for IRType {
    fn extract(ob: &'source PyAny) -> PyResult<Self> {
        py_string_to_enum(&IR_TYPES, ob)
    }
}

#[derive(Debug)]
pub enum Const {
    U1(bool),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F32i(u32),
    F64(f64),
    F64i(u64),
    V128(u16),
    V256(u32),
}

#[derive(Debug)]
pub enum Expr {
    Get {
        offset: i32,
        ty: IRType,
    },
    // TODO
    GetI {},
    RdTmp(IRTemp),
    Op1 {
        op: Op1,
        arg: Box<Expr>,
    },
    Op2 {
        op: Op2,
        args: [Box<Expr>; 2],
    },
    Op3 {
        op: Op3,
        args: [Box<Expr>; 3],
    },
    Op4 {
        op: String,
        args: [Box<Expr>; 4],
    },
    Load {
        endianness: Endianness,
        ty: IRType,
        addr: Box<Expr>,
    },
    Const(Const),
    /// Call to a pure C function
    CCall {
        // TODO
        func: (),
        return_ty: IRType,
        args: Vec<Expr>,
    },
    /// If-then-else
    Ite {
        condition: Box<Expr>,
        true_value: Box<Expr>,
        false_value: Box<Expr>,
    },
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug)]
pub enum JumpKind {
    Boring,
    Call,
    Ret,
    ClientReq,
    Yield,
    EmWarn,
    EmFail,
    NoDecode,
    MapFail,
    InvalICache,
    FlushDCache,
    NoRedir,
    SigILL,
    SigTRAP,
    SigSEGV,
    SigBUS,
    SigFPE,
    SigFPE_IntDiv,
    SigFPE_IntOvf,
    Privileged,
    Sys_syscall,
    Sys_int32,
    Sys_int128,
    Sys_int129,
    Sys_int130,
    Sys_int145,
    Sys_int210,
    Sys_sysenter,
}

const JUMP_KINDS: PhfStringMap<JumpKind> = phf_map! {
    "Ijk_Boring" => JumpKind::Boring,
    "Ijk_Call" => JumpKind::Call,
    "Ijk_Ret" => JumpKind::Ret,
    "Ijk_ClientReq" => JumpKind::ClientReq,
    "Ijk_Yield" => JumpKind::Yield,
    "Ijk_EmWarn" => JumpKind::EmWarn,
    "Ijk_EmFail" => JumpKind::EmFail,
    "Ijk_NoDecode" => JumpKind::NoDecode,
    "Ijk_MapFail" => JumpKind::MapFail,
    "Ijk_InvalICache" => JumpKind::InvalICache,
    "Ijk_FlushDCache" => JumpKind::FlushDCache,
    "Ijk_NoRedir" => JumpKind::NoRedir,
    "Ijk_SigILL" => JumpKind::SigILL,
    "Ijk_SigTRAP" => JumpKind::SigTRAP,
    "Ijk_SigSEGV" => JumpKind::SigSEGV,
    "Ijk_SigBUS" => JumpKind::SigBUS,
    "Ijk_SigFPE" => JumpKind::SigFPE,
    "Ijk_SigFPE_IntDiv" => JumpKind::SigFPE_IntDiv,
    "Ijk_SigFPE_IntOvf" => JumpKind::SigFPE_IntOvf,
    "Ijk_Privileged" => JumpKind::Privileged,
    "Ijk_Sys_syscall" => JumpKind::Sys_syscall,
    "Ijk_Sys_int32" => JumpKind::Sys_int32,
    "Ijk_Sys_int128" => JumpKind::Sys_int128,
    "Ijk_Sys_int129" => JumpKind::Sys_int129,
    "Ijk_Sys_int130" => JumpKind::Sys_int130,
    "Ijk_Sys_int145" => JumpKind::Sys_int145,
    "Ijk_Sys_int210" => JumpKind::Sys_int210,
    "Ijk_Sys_sysenter" => JumpKind::Sys_sysenter,
};

impl<'source> FromPyObject<'source> for JumpKind {
    fn extract(ob: &'source PyAny) -> PyResult<Self> {
        py_string_to_enum(&JUMP_KINDS, ob)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Endianness {
    Little,
    Big,
}

const ENDIANNESSES: PhfStringMap<Endianness> = phf_map! {
    "Iend_LE" => Endianness::Little,
    "Iend_BE" => Endianness::Big,
};

impl<'source> FromPyObject<'source> for Endianness {
    fn extract(ob: &'source PyAny) -> PyResult<Self> {
        py_string_to_enum(&ENDIANNESSES, ob)
    }
}

#[derive(Debug)]
pub enum Statement {
    NoOp,
    IMark {
        addr: Addr64,
        len: u32,
        delta: u8,
    },
    // TODO
    AbiHint,
    Put {
        offset: i32,
        data: Expr,
    },
    // TODO
    PutI,
    WrTmp {
        tmp: IRTemp,
        data: Expr,
    },
    Store {
        endianness: Endianness,
        addr: Expr,
        data: Expr,
    },
    // TODO
    CAS,
    // TODO
    LLSC,
    // TODO
    MemoryBusEvent,
    // TODO
    Dirty,
    /// Conditional ("guarded") exit
    Exit {
        guard: Expr,
        dst: Const,
        jump_kind: JumpKind,
        offset_ip: i32,
    },
    // TODO
    /// Conditional ("guarded") load
    LoadG,
    // TODO
    /// Conditional ("guarded") store
    StoreG,
}

#[derive(Debug)]
pub struct Block {
    statements: Vec<Statement>,
    next: Expr,
    jump_kind: JumpKind,
}

#[allow(non_snake_case)]
pub struct IrConverter<'a> {
    tag_to_stmt_class: &'a PyFunction,
    stmt_type_NoOp: &'a PyType,
    stmt_type_IMark: &'a PyType,
    stmt_type_abihint: &'a PyType,
    stmt_type_put: &'a PyType,
    stmt_type_puti: &'a PyType,
    stmt_type_wrtmp: &'a PyType,
    stmt_type_store: &'a PyType,
    stmt_type_cas: &'a PyType,
    stmt_type_llsc: &'a PyType,
    stmt_type_mbe: &'a PyType,
    stmt_type_dirty: &'a PyType,
    stmt_type_exit: &'a PyType,
    stmt_type_loadg: &'a PyType,
    stmt_type_storeg: &'a PyType,

    tag_to_expr_class: &'a PyFunction,
    expr_type_Get: &'a PyType,
    expr_type_GetI: &'a PyType,
    expr_type_RdTmp: &'a PyType,
    expr_type_Qop: &'a PyType,
    expr_type_Triop: &'a PyType,
    expr_type_Binop: &'a PyType,
    expr_type_Unop: &'a PyType,
    expr_type_Load: &'a PyType,
    expr_type_Const: &'a PyType,
    expr_type_CCall: &'a PyType,
    expr_type_ITE: &'a PyType,

    tag_to_const_class: &'a PyFunction,
    const_type_U1: &'a PyType,
    const_type_U8: &'a PyType,
    const_type_U16: &'a PyType,
    const_type_U32: &'a PyType,
    const_type_U64: &'a PyType,
    const_type_F32: &'a PyType,
    const_type_F32i: &'a PyType,
    const_type_F64: &'a PyType,
    const_type_F64i: &'a PyType,
    const_type_V128: &'a PyType,
    const_type_V256: &'a PyType,
}

impl<'a> IrConverter<'a> {
    pub fn new(pyvex: &'a PyModule) -> PyResult<Self> {
        let pyvex_stmt = pyvex.getattr("stmt")?;
        let tag_to_stmt_class: &PyFunction = pyvex_stmt.getattr("tag_to_stmt_class")?.cast_as()?;

        let pyvex_expr = pyvex.getattr("expr")?;
        let tag_to_expr_class: &PyFunction = pyvex_expr.getattr("tag_to_expr_class")?.cast_as()?;

        let pyvex_const = pyvex.getattr("const")?;
        let tag_to_const_class: &PyFunction =
            pyvex_const.getattr("tag_to_const_class")?.cast_as()?;

        Ok(Self {
            tag_to_stmt_class,
            stmt_type_NoOp: pyvex_stmt.getattr("NoOp")?.cast_as()?,
            stmt_type_IMark: pyvex_stmt.getattr("IMark")?.cast_as()?,
            stmt_type_abihint: pyvex_stmt.getattr("AbiHint")?.cast_as()?,
            stmt_type_put: pyvex_stmt.getattr("Put")?.cast_as()?,
            stmt_type_puti: pyvex_stmt.getattr("PutI")?.cast_as()?,
            stmt_type_wrtmp: pyvex_stmt.getattr("WrTmp")?.cast_as()?,
            stmt_type_store: pyvex_stmt.getattr("Store")?.cast_as()?,
            stmt_type_cas: pyvex_stmt.getattr("CAS")?.cast_as()?,
            stmt_type_llsc: pyvex_stmt.getattr("LLSC")?.cast_as()?,
            stmt_type_mbe: pyvex_stmt.getattr("MBE")?.cast_as()?,
            stmt_type_dirty: pyvex_stmt.getattr("Dirty")?.cast_as()?,
            stmt_type_exit: pyvex_stmt.getattr("Exit")?.cast_as()?,
            stmt_type_loadg: pyvex_stmt.getattr("LoadG")?.cast_as()?,
            stmt_type_storeg: pyvex_stmt.getattr("StoreG")?.cast_as()?,

            tag_to_expr_class,
            expr_type_Get: pyvex_expr.getattr("Get")?.cast_as()?,
            expr_type_GetI: pyvex_expr.getattr("GetI")?.cast_as()?,
            expr_type_RdTmp: pyvex_expr.getattr("RdTmp")?.cast_as()?,
            expr_type_Qop: pyvex_expr.getattr("Qop")?.cast_as()?,
            expr_type_Triop: pyvex_expr.getattr("Triop")?.cast_as()?,
            expr_type_Binop: pyvex_expr.getattr("Binop")?.cast_as()?,
            expr_type_Unop: pyvex_expr.getattr("Unop")?.cast_as()?,
            expr_type_Load: pyvex_expr.getattr("Load")?.cast_as()?,
            expr_type_Const: pyvex_expr.getattr("Const")?.cast_as()?,
            expr_type_CCall: pyvex_expr.getattr("CCall")?.cast_as()?,
            expr_type_ITE: pyvex_expr.getattr("ITE")?.cast_as()?,

            tag_to_const_class,
            const_type_U1: pyvex_const.getattr("U1")?.cast_as()?,
            const_type_U8: pyvex_const.getattr("U8")?.cast_as()?,
            const_type_U16: pyvex_const.getattr("U16")?.cast_as()?,
            const_type_U32: pyvex_const.getattr("U32")?.cast_as()?,
            const_type_U64: pyvex_const.getattr("U64")?.cast_as()?,
            const_type_F32: pyvex_const.getattr("F32")?.cast_as()?,
            const_type_F32i: pyvex_const.getattr("F32i")?.cast_as()?,
            const_type_F64: pyvex_const.getattr("F64")?.cast_as()?,
            const_type_F64i: pyvex_const.getattr("F64i")?.cast_as()?,
            const_type_V128: pyvex_const.getattr("V128")?.cast_as()?,
            const_type_V256: pyvex_const.getattr("V256")?.cast_as()?,
        })
    }

    pub fn convert_block(&self, block: &PyAny) -> PyResult<Block> {
        Ok(Block {
            statements: block
                .getattr("statements")?
                .iter()?
                .map(|stmt| self.convert_stmt(stmt?))
                .collect::<PyResult<Vec<_>>>()?,
            next: self.convert_expr(block.getattr("next")?)?,
            jump_kind: block.getattr("jumpkind")?.extract()?,
        })
    }

    pub fn convert_stmt(&self, stmt: &PyAny) -> PyResult<Statement> {
        let tag = stmt.getattr("tag")?;
        let stmt_class = self.tag_to_stmt_class.call1((tag,))?;

        let stmt = if stmt_class.is(self.stmt_type_NoOp) {
            Statement::NoOp
        } else if stmt_class.is(self.stmt_type_IMark) {
            Statement::IMark {
                addr: stmt.getattr("addr")?.extract()?,
                len: stmt.getattr("len")?.extract()?,
                delta: stmt.getattr("delta")?.extract()?,
            }
        } else if stmt_class.is(self.stmt_type_abihint) {
            Statement::AbiHint
        } else if stmt_class.is(self.stmt_type_put) {
            Statement::Put {
                offset: stmt.getattr("offset")?.extract()?,
                data: self.convert_expr(stmt.getattr("data")?)?,
            }
        } else if stmt_class.is(self.stmt_type_puti) {
            Statement::PutI
        } else if stmt_class.is(self.stmt_type_wrtmp) {
            Statement::WrTmp {
                tmp: stmt.getattr("tmp")?.extract()?,
                data: self.convert_expr(stmt.getattr("data")?)?,
            }
        } else if stmt_class.is(self.stmt_type_store) {
            Statement::Store {
                endianness: stmt.getattr("end")?.extract()?,
                addr: self.convert_expr(stmt.getattr("addr")?)?,
                data: self.convert_expr(stmt.getattr("data")?)?,
            }
        } else if stmt_class.is(self.stmt_type_cas) {
            Statement::CAS
        } else if stmt_class.is(self.stmt_type_llsc) {
            Statement::LLSC
        } else if stmt_class.is(self.stmt_type_mbe) {
            Statement::MemoryBusEvent
        } else if stmt_class.is(self.stmt_type_dirty) {
            Statement::Dirty
        } else if stmt_class.is(self.stmt_type_exit) {
            Statement::Exit {
                guard: self.convert_expr(stmt.getattr("guard")?)?,
                dst: self.convert_const(stmt.getattr("dst")?)?,
                jump_kind: stmt.getattr("jk")?.extract()?,
                offset_ip: stmt.getattr("offsIP")?.extract()?,
            }
        } else if stmt_class.is(self.stmt_type_loadg) {
            Statement::LoadG
        } else if stmt_class.is(self.stmt_type_storeg) {
            Statement::StoreG
        } else {
            unimplemented!("unknown statement type for {:?}", stmt);
        };

        Ok(stmt)
    }

    fn convert_op_and_args<'b, Op: FromPyObject<'b>, const N: usize>(
        &self,
        expr: &'b PyAny,
    ) -> PyResult<(Op, [Box<Expr>; N])> {
        let op = expr.getattr("op")?.extract()?;
        let args: [&PyAny; N] = expr.getattr("args")?.extract()?;
        let args = ArrayExt::try_map(args, |arg| self.convert_expr(arg).map(Box::new))?;
        Ok((op, args))
    }

    pub fn convert_expr(&self, expr: &PyAny) -> PyResult<Expr> {
        let tag = expr.getattr("tag")?;
        let expr_class = self.tag_to_expr_class.call1((tag,))?;

        let expr = if expr_class.is(self.expr_type_Get) {
            Expr::Get {
                offset: expr.getattr("offset")?.extract()?,
                ty: expr.getattr("ty")?.extract()?,
            }
        } else if expr_class.is(self.expr_type_GetI) {
            Expr::GetI {}
        } else if expr_class.is(self.expr_type_RdTmp) {
            Expr::RdTmp(expr.getattr("tmp")?.extract()?)
        } else if expr_class.is(self.expr_type_Qop) {
            let (op, args) = self.convert_op_and_args(expr)?;
            Expr::Op4 { op, args }
        } else if expr_class.is(self.expr_type_Triop) {
            let (op, args) = self.convert_op_and_args(expr)?;
            Expr::Op3 { op, args }
        } else if expr_class.is(self.expr_type_Binop) {
            let (op, args) = self.convert_op_and_args(expr)?;
            Expr::Op2 { op, args }
        } else if expr_class.is(self.expr_type_Unop) {
            let (op, args) = self.convert_op_and_args(expr)?;
            let [arg] = args;
            Expr::Op1 { op, arg }
        } else if expr_class.is(self.expr_type_Load) {
            Expr::Load {
                endianness: expr.getattr("end")?.extract()?,
                ty: expr.getattr("ty")?.extract()?,
                addr: Box::new(self.convert_expr(expr.getattr("addr")?)?),
            }
        } else if expr_class.is(self.expr_type_Const) {
            Expr::Const(self.convert_const(expr.getattr("con")?)?)
        } else if expr_class.is(self.expr_type_CCall) {
            Expr::CCall {
                func: (),
                return_ty: expr.getattr("return_ty")?.extract()?,
                args: expr
                    .getattr("args")?
                    .iter()?
                    .map(|arg| self.convert_expr(arg?))
                    .collect::<PyResult<Vec<Expr>>>()?,
            }
        } else if expr_class.is(self.expr_type_ITE) {
            Expr::Ite {
                condition: Box::new(self.convert_expr(expr.getattr("cond")?)?),
                true_value: Box::new(self.convert_expr(expr.getattr("iftrue")?)?),
                false_value: Box::new(self.convert_expr(expr.getattr("iffalse")?)?),
            }
        } else {
            unimplemented!("unknown expression type for {:?}", expr);
        };

        Ok(expr)
    }

    pub fn convert_const(&self, value: &PyAny) -> PyResult<Const> {
        let tag = value.getattr("tag")?;
        let const_class = self.tag_to_const_class.call1((tag,))?;

        let inner = value.getattr("value")?;
        let value = if const_class.is(self.const_type_U1) {
            Const::U1(inner.extract()?)
        } else if const_class.is(self.const_type_U8) {
            Const::U8(inner.extract()?)
        } else if const_class.is(self.const_type_U16) {
            Const::U16(inner.extract()?)
        } else if const_class.is(self.const_type_U32) {
            Const::U32(inner.extract()?)
        } else if const_class.is(self.const_type_U64) {
            Const::U64(inner.extract()?)
        } else if const_class.is(self.const_type_F32) {
            Const::F32(inner.extract()?)
        } else if const_class.is(self.const_type_F32i) {
            Const::F32i(inner.extract()?)
        } else if const_class.is(self.const_type_F64) {
            Const::F64(inner.extract()?)
        } else if const_class.is(self.const_type_F64i) {
            Const::F64i(inner.extract()?)
        } else if const_class.is(self.const_type_V128) {
            Const::V128(inner.extract()?)
        } else if const_class.is(self.const_type_V256) {
            Const::V256(inner.extract()?)
        } else {
            unimplemented!("unknown const value type for {:?}", value);
        };

        Ok(value)
    }
}