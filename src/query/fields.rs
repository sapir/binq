use crate::{
    analysis::data_flow::ValueSources,
    database::StatementAddr,
    ir::{Expr as IrExpr, SimpleExpr, Statement},
};

use super::expr::{Expr, ExprMatcher};

#[derive(Clone, Copy, Debug)]
pub enum Field {
    Value,
    StoreAddr,
    Target,
    Condition,
}

#[derive(Clone, Debug)]
pub struct ExprMatchFilter {
    pub field: Field,
    pub expr: Expr,
}

enum MaybeSimpleExpr<'a> {
    Simple(&'a SimpleExpr),
    Complex(&'a IrExpr),
}

fn get_field(stmt: &Statement, field: Field) -> Option<MaybeSimpleExpr> {
    match (stmt, field) {
        (Statement::Nop, _) => None,

        (Statement::Assign { rhs, .. }, Field::Value) => Some(MaybeSimpleExpr::Complex(rhs)),
        (Statement::Assign { .. }, _) => None,

        (Statement::Store { addr, .. }, Field::StoreAddr) => Some(MaybeSimpleExpr::Simple(addr)),
        (Statement::Store { value, .. }, Field::Value) => Some(MaybeSimpleExpr::Simple(value)),
        (Statement::Store { .. }, _) => None,

        (Statement::Call { target } | Statement::Jump { target, .. }, Field::Target) => {
            Some(MaybeSimpleExpr::Simple(target))
        }

        (
            Statement::Jump {
                condition: Some(condition),
                ..
            },
            Field::Condition,
        ) => Some(MaybeSimpleExpr::Simple(condition)),

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

    let Some(ir_expr) = get_field(stmt, *field) else { return false };

    match ir_expr {
        MaybeSimpleExpr::Simple(ir_expr) => {
            matcher.match_simple_expr(addr, value_sources, expr, ir_expr)
        }

        MaybeSimpleExpr::Complex(ir_expr) => matcher.match_expr(addr, value_sources, expr, ir_expr),
    }
}
