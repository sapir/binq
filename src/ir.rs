use std::{
    fmt::{self, Display},
    sync::atomic::{AtomicU64, Ordering},
};

pub type Addr64 = u64;

pub type Const = u64;

static NEXT_TEMP: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Temp(pub u64);

impl Temp {
    pub fn new() -> Self {
        Self(NEXT_TEMP.fetch_add(1, Ordering::SeqCst))
    }

    pub(crate) fn peek_next_id() -> u64 {
        NEXT_TEMP.load(Ordering::SeqCst)
    }
}

impl Display for Temp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Register(pub u16);

impl Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "r{:x}", self.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

impl Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Register(r) => r.fmt(f),
            Variable::Temp(t) => t.fmt(f),
        }
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

impl Display for SimpleExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SimpleExpr::Const(x) => write!(f, "{:#x}", x),
            SimpleExpr::Variable(x) => x.fmt(f),
        }
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

impl Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.op {
            UnaryOpKind::Not => write!(f, "!{}", self.value),
        }
    }
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

impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op_str = match self.op {
            BinaryOpKind::Add => "+",
            BinaryOpKind::Sub => "-",
            BinaryOpKind::Mul => "*",
            BinaryOpKind::Shl => "shl",
            BinaryOpKind::Shr => "shr",
            BinaryOpKind::Sar => "sar",
            BinaryOpKind::Rol => "rol",
            BinaryOpKind::Ror => "ror",
            BinaryOpKind::And => "&",
            BinaryOpKind::Or => "|",
            BinaryOpKind::Xor => "^",
        };

        write!(f, "{} {} {}", self.lhs, op_str, self.rhs)
    }
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

impl Display for CompareOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op_str = match self.kind {
            CompareOpKind::Equal => "==",
            CompareOpKind::NotEqual => "!=",
            CompareOpKind::LessThanUnsigned => "ltu",
            CompareOpKind::LessThanOrEqualUnsigned => "leu",
            CompareOpKind::GreaterThanUnsigned => "gtu",
            CompareOpKind::GreaterThanOrEqualUnsigned => "geu",
            CompareOpKind::LessThanSigned => "lts",
            CompareOpKind::LessThanOrEqualSigned => "les",
            CompareOpKind::GreaterThanSigned => "gts",
            CompareOpKind::GreaterThanOrEqualSigned => "ges",
        };

        write!(f, "({} {} {})", self.lhs, op_str, self.rhs)
    }
}

/// An X86 condition code that involves multiple flags
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ComplexX86ConditionCode {
    Be,
    L,
    Le,
}

impl Display for ComplexX86ConditionCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ComplexX86ConditionCode::Be => "x86_be()",
                ComplexX86ConditionCode::L => "x86_l()",
                ComplexX86ConditionCode::Le => "x86_le()",
            }
        )
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Unknown,
    Simple(SimpleExpr),
    Deref(SimpleExpr),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    CompareOp(CompareOp),
    /// `lhs` with `num_bits` bits shifted left by `shift`, replaced by `rhs`
    /// (also shifted left by `shift`)
    InsertBits {
        lhs: SimpleExpr,
        shift: u8,
        num_bits: u8,
        rhs: SimpleExpr,
    },
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

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Unknown => write!(f, "unknown"),
            Expr::Simple(x) => x.fmt(f),
            Expr::Deref(x) => write!(f, "*{}", x),
            Expr::UnaryOp(op) => op.fmt(f),
            Expr::BinaryOp(op) => op.fmt(f),
            Expr::CompareOp(op) => op.fmt(f),
            Expr::InsertBits {
                lhs,
                shift,
                num_bits,
                rhs,
            } => write!(f, "insert_bits({}, {}, {}, {})", lhs, shift, num_bits, rhs),
            Expr::X86Flag {
                flag_reg,
                from_expr,
            } => write!(f, "x86_flag({}, {})", flag_reg, from_expr),
            Expr::ComplexX86ConditionCode(cc) => cc.fmt(f),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Statement {
    Nop,

    Assign {
        lhs: Variable,
        rhs: Expr,
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

    ClearTemps,
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Nop => write!(f, "nop"),
            Statement::Assign { lhs, rhs } => write!(f, "{} = {}", lhs, rhs),
            Statement::Store { addr, value } => write!(f, "*{} = {}", addr, value),
            Statement::Call { target } => write!(f, "call {}", target),
            Statement::Jump {
                target,
                is_return,
                condition,
            } => {
                if *is_return {
                    write!(f, "ret to {}", target)?;
                } else {
                    write!(f, "jmp {}", target)?;
                }

                if let Some(condition) = condition {
                    write!(f, " if {}", condition)?;
                }

                Ok(())
            }
            Statement::Intrinsic => write!(f, "intrinsic"),
            Statement::ClearTemps => write!(f, "clear_temps"),
        }
    }
}
