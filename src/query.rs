mod expr;
mod fields;

use crate::{
    analysis::data_flow::ValueSources,
    database::{Database, StatementAddr},
    ir::Statement,
};

use self::fields::match_expr_filter;

pub use self::{
    expr::{ConditionExpr, Expr, ExprKind, ExprMatch},
    fields::{ExprMatchFilter, Field},
};

pub fn search(database: &mut Database, filters: &[ExprMatchFilter]) -> Vec<ExprMatch> {
    let mut query = database.world.query::<(&Statement, &ValueSources)>();
    let view = query.view();

    database
        .world
        .query::<(&StatementAddr, &Statement, &ValueSources)>()
        .into_iter()
        .filter_map(|(_entity, (addr, stmt, value_sources))| {
            let mut captures = vec![];
            for filter in filters {
                captures.extend(match_expr_filter(
                    database,
                    &view,
                    *addr,
                    stmt,
                    value_sources,
                    filter,
                )?);
            }
            Some(ExprMatch {
                addr: *addr,
                captures,
            })
        })
        .collect()
}
