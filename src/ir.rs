use std::sync::atomic::{AtomicU64, Ordering};

pub type Addr64 = u64;

pub type Const = u64;

static NEXT_TEMP: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Temp(pub u64);

impl Temp {
    pub fn new() -> Self {
        Self(NEXT_TEMP.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Register(pub u16);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Variable {
    Register(Register),
    Temp(Temp),
}

impl Variable {
    pub fn new_temp() -> Self {
        Self::Temp(Temp::new())
    }
}

impl From<Register> for Variable {
    fn from(reg: Register) -> Self {
        Self::Register(reg)
    }
}

impl From<Temp> for Variable {
    fn from(temp: Temp) -> Self {
        Self::Temp(temp)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SimpleExpr {
    Const(Const),
    Variable(Variable),
}

impl SimpleExpr {
    pub fn as_const(self) -> Option<Const> {
        match self {
            SimpleExpr::Const(x) => Some(x),
            _ => None,
        }
    }
}

impl From<Const> for SimpleExpr {
    fn from(value: Const) -> Self {
        Self::Const(value)
    }
}

impl From<Register> for SimpleExpr {
    fn from(reg: Register) -> Self {
        Self::Variable(reg.into())
    }
}

impl From<Variable> for SimpleExpr {
    fn from(value: Variable) -> Self {
        Self::Variable(value)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOpKind {
    Not,
}

#[derive(Clone, Debug)]
pub struct UnaryOp {
    pub op: UnaryOpKind,
    pub value: SimpleExpr,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Shl,
    Shr,
    Sar,
    Rol,
    Ror,
    And,
    Or,
    Xor,
}

#[derive(Clone, Debug)]
pub struct BinaryOp {
    pub op: BinaryOpKind,
    pub lhs: SimpleExpr,
    pub rhs: SimpleExpr,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CompareOpKind {
    Equal,
    NotEqual,
    LessThanUnsigned,
    LessThanOrEqualUnsigned,
    GreaterThanUnsigned,
    GreaterThanOrEqualUnsigned,
    LessThanSigned,
    LessThanOrEqualSigned,
    GreaterThanSigned,
    GreaterThanOrEqualSigned,
}

#[derive(Clone, Debug)]
pub struct CompareOp {
    pub kind: CompareOpKind,
    pub lhs: SimpleExpr,
    pub rhs: SimpleExpr,
}

/// An X86 condition code that involves multiple flags
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ComplexX86ConditionCode {
    Be,
    L,
    Le,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Unknown,
    Simple(SimpleExpr),
    Deref(SimpleExpr),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    CompareOp(CompareOp),
    X86Flag {
        flag_reg: Register,
        from_expr: Variable,
    },
    ComplexX86ConditionCode(ComplexX86ConditionCode),
}

impl<T> From<T> for Expr
where
    SimpleExpr: From<T>,
{
    fn from(value: T) -> Self {
        Self::Simple(value.into())
    }
}

#[derive(Clone, Debug)]
pub enum Statement {
    Nop,

    Assign {
        lhs: Variable,
        rhs: Expr,
    },

    InsertBits {
        reg: Register,
        shift: u8,
        num_bits: u8,
        value: SimpleExpr,
    },

    Store {
        addr: SimpleExpr,
        value: SimpleExpr,
    },

    // TODO: abi info?
    Call {
        target: SimpleExpr,
    },

    Jump {
        target: SimpleExpr,
        is_return: bool,
        condition: Option<SimpleExpr>,
    },

    // TODO: affected regs
    Intrinsic,
}
