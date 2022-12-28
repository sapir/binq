use std::str::FromStr;

use iced_x86::Register as X86Register;

use crate::{
    analysis::data_flow::ValueSources,
    database::{ArchAndAbi, StatementAddr},
    ir::{Expr as IrExpr, SimpleExpr, Statement},
    lifting::wrap_x86_reg,
};

use super::expr::{Expr, ExprMatcher};

const X64_CALL_REG_ARGS: [X86Register; 6] = [
    X86Register::RDI,
    X86Register::RSI,
    X86Register::RDX,
    X86Register::RCX,
    X86Register::R8,
    X86Register::R9,
];

const X64_WINDOWS_CALL_REG_ARGS: [X86Register; 4] = [
    X86Register::RCX,
    X86Register::RDX,
    X86Register::R8,
    X86Register::R9,
];

#[derive(Clone, Copy, Debug)]
pub enum Field {
    Value,
    StoreAddr,
    Target,
    Condition,
    CallArg(usize),
}

impl FromStr for Field {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "value" => Ok(Self::Value),
            "store_addr" => Ok(Self::StoreAddr),
            "target" => Ok(Self::Target),
            "condition" => Ok(Self::Condition),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprMatchFilter {
    pub field: Field,
    pub expr: Expr,
}

enum MaybeSimpleExpr<'a> {
    Simple(SimpleExpr),
    Complex(&'a IrExpr),
}

fn get_field(arch_and_abi: ArchAndAbi, stmt: &Statement, field: Field) -> Option<MaybeSimpleExpr> {
    match (stmt, field) {
        (Statement::Nop, _) => None,

        (Statement::Assign { rhs, .. }, Field::Value) => Some(MaybeSimpleExpr::Complex(rhs)),
        (Statement::Assign { .. }, _) => None,

        (Statement::Store { addr, .. }, Field::StoreAddr) => Some(MaybeSimpleExpr::Simple(*addr)),
        (Statement::Store { value, .. }, Field::Value) => Some(MaybeSimpleExpr::Simple(*value)),
        (Statement::Store { .. }, _) => None,

        (Statement::Call { target } | Statement::Jump { target, .. }, Field::Target) => {
            Some(MaybeSimpleExpr::Simple(*target))
        }

        (
            Statement::Jump {
                condition: Some(condition),
                ..
            },
            Field::Condition,
        ) => Some(MaybeSimpleExpr::Simple(*condition)),

        (Statement::Call { .. }, Field::CallArg(arg_index)) => match arch_and_abi {
            ArchAndAbi::X86 => todo!("x86 call arguments"),

            ArchAndAbi::X64 | ArchAndAbi::X64Windows => {
                let reg_args = match arch_and_abi {
                    ArchAndAbi::X64 => &X64_CALL_REG_ARGS[..],
                    ArchAndAbi::X64Windows => &X64_WINDOWS_CALL_REG_ARGS[..],
                    _ => unreachable!(),
                };
                let reg = *reg_args.get(arg_index).unwrap_or_else(|| {
                    todo!("only register arguments are currently supported");
                });

                Some(MaybeSimpleExpr::Simple(wrap_x86_reg(reg).into()))
            }
        },

        (Statement::Call { .. } | Statement::Jump { .. }, _) => None,

        (Statement::Intrinsic | Statement::ClearTemps, _) => None,
    }
}

pub(super) fn match_expr_filter(
    matcher: &mut ExprMatcher,
    addr: StatementAddr,
    stmt: &Statement,
    value_sources: &ValueSources,
    filter: &ExprMatchFilter,
) -> bool {
    let ExprMatchFilter { field, expr } = filter;

    let Some(ir_expr) = get_field(matcher.database().arch_and_abi, stmt, *field) else { return false };

    match ir_expr {
        MaybeSimpleExpr::Simple(ir_expr) => {
            matcher.match_simple_expr(addr, value_sources, expr, &ir_expr)
        }

        MaybeSimpleExpr::Complex(ir_expr) => matcher.match_expr(addr, value_sources, expr, ir_expr),
    }
}
