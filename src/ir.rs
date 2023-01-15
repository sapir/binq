use std::{
    fmt::{self, Display},
    sync::atomic::{AtomicU64, Ordering},
};

use iced_x86::Mnemonic;

pub type Addr64 = u64;

pub type Const = u64;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SizeBits(pub u32);

impl SizeBits {
    pub fn bits(self) -> u32 {
        self.0
    }
}

impl From<SizeBytes> for SizeBits {
    fn from(value: SizeBytes) -> Self {
        Self(value.0 * 8)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SizeBytes(pub u32);

impl SizeBytes {
    pub fn bits(self) -> u32 {
        SizeBits::from(self).bits()
    }
}

impl TryFrom<SizeBits> for SizeBytes {
    type Error = ();

    fn try_from(value: SizeBits) -> Result<Self, Self::Error> {
        if value.0 % 8 == 0 {
            Ok(Self(value.0 / 8))
        } else {
            Err(())
        }
    }
}

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

    pub fn register(self) -> Option<Register> {
        if let Variable::Register(reg) = self {
            Some(reg)
        } else {
            None
        }
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
            SimpleExpr::Const(x) => write!(f, "{x:#x}"),
            SimpleExpr::Variable(x) => x.fmt(f),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ChangeWidthKind {
    Truncate,
    ZeroExtend,
    SignExtend,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ChangeWidthOp {
    pub kind: ChangeWidthKind,
    pub new_size: SizeBits,
    /// This is expected to always be a `SimpleExpr::Variable`. We could use a
    /// `Variable` here instead of a `SimpleExpr` but it's easier for code
    /// handling it if it's a `SimpleExpr` like everything else.
    pub inner: SimpleExpr,
}

impl Display for ChangeWidthOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}(u{}, {})",
            match self.kind {
                ChangeWidthKind::Truncate => "truncate",
                ChangeWidthKind::ZeroExtend => "zero_extend",
                ChangeWidthKind::SignExtend => "sign_extend",
            },
            self.new_size.bits(),
            self.inner
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOpKind {
    Not,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
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
    LessThanSigned,
    LessThanOrEqualSigned,
}

impl CompareOpKind {
    pub fn is_symmetric(self) -> bool {
        match self {
            CompareOpKind::Equal | CompareOpKind::NotEqual => true,

            CompareOpKind::LessThanUnsigned
            | CompareOpKind::LessThanOrEqualUnsigned
            | CompareOpKind::LessThanSigned
            | CompareOpKind::LessThanOrEqualSigned => false,
        }
    }

    /// Get comparison kind that is opposite to this one, if you swap the left-
    /// and right-hand sides of the comparison. e.g. `x < 5` iff `!(5 <= x)`.
    pub fn swapped_inverse(self) -> Self {
        use CompareOpKind::*;

        match self {
            Equal => NotEqual,
            NotEqual => Equal,
            LessThanUnsigned => LessThanOrEqualUnsigned,
            LessThanOrEqualUnsigned => LessThanUnsigned,
            LessThanSigned => LessThanOrEqualSigned,
            LessThanOrEqualSigned => LessThanSigned,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
            CompareOpKind::LessThanSigned => "lts",
            CompareOpKind::LessThanOrEqualSigned => "les",
        };

        write!(f, "({} {} {})", self.lhs, op_str, self.rhs)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum X86Flag {
    /// Overflow Flag
    OF,
    /// Sign Flag
    SF,
    /// Zero Flag
    ZF,
    /// Carry Flag
    CF,
}

impl Display for X86Flag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct X86FlagResult {
    pub which_flag: X86Flag,
    /// The mnemonic that generated this flag. If the original mnemonic was
    /// `Cmp` or `Test`, then `Sub` or `And` will be used instead.
    pub mnemonic: Mnemonic,
    pub lhs: SimpleExpr,
    pub rhs: SimpleExpr,
    pub math_result: Variable,
}

impl Display for X86FlagResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: use an iced formatter for the mnemonic?
        write!(
            f,
            "x86_flag({}, {:?}, {}, {}, {})",
            self.which_flag, self.mnemonic, self.lhs, self.rhs, self.math_result
        )
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Unknown,
    Simple(SimpleExpr),
    ChangeWidth(ChangeWidthOp),
    Deref {
        ptr: SimpleExpr,
        size: SizeBytes,
    },
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    CompareOp(CompareOp),
    /// `lhs`, with `num_bits` bits shifted left by `shift` replaced by `rhs`
    /// (which is also shifted left by `shift`).
    InsertBits {
        lhs: SimpleExpr,
        shift: u32,
        num_bits: u32,
        rhs: SimpleExpr,
    },
    /// Get `num_bits` bits from `inner`, starting at bit number `shift`.
    ExtractBits {
        inner: SimpleExpr,
        shift: u32,
        num_bits: u32,
    },
    X86Flag(X86FlagResult),
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
            Expr::Deref { ptr, size } => {
                let size_bits = size.bits();
                write!(f, "*(u{size_bits}*){ptr}")
            }
            Expr::UnaryOp(op) => op.fmt(f),
            Expr::BinaryOp(op) => op.fmt(f),
            Expr::CompareOp(op) => op.fmt(f),
            Expr::ChangeWidth(op) => op.fmt(f),
            Expr::InsertBits {
                lhs,
                shift,
                num_bits,
                rhs,
            } => write!(f, "insert_bits({lhs}, {shift}, {num_bits}, {rhs})"),
            Expr::ExtractBits {
                inner,
                shift,
                num_bits,
            } => write!(f, "extract_bits({inner}, {shift}, {num_bits})"),
            Expr::X86Flag(flag_result) => flag_result.fmt(f),
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
        size: SizeBytes,
    },

    Call {
        target: SimpleExpr,
    },

    Jump {
        target: SimpleExpr,
        is_return: bool,
        condition: Option<SimpleExpr>,
    },

    Intrinsic,

    ClearTemps,
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Nop => write!(f, "nop"),
            Statement::Assign { lhs, rhs } => write!(f, "{lhs} = {rhs}"),
            Statement::Store { addr, value, size } => {
                let size_bits = size.bits();
                write!(f, "*(u{size_bits}*){addr} = {value}")
            }
            Statement::Call { target } => write!(f, "call {target}"),
            Statement::Jump {
                target,
                is_return,
                condition,
            } => {
                if *is_return {
                    write!(f, "ret to {target}")?;
                } else {
                    write!(f, "jmp {target}")?;
                }

                if let Some(condition) = condition {
                    write!(f, " if {condition}")?;
                }

                Ok(())
            }
            Statement::Intrinsic => write!(f, "intrinsic"),
            Statement::ClearTemps => write!(f, "clear_temps"),
        }
    }
}
