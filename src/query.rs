mod expr;
mod fields;

use crate::{
    analysis::data_flow::ValueSources,
    database::{Database, StatementAddr},
    ir::Statement,
};

use self::{
    expr::{Captures, ExprMatcher},
    fields::match_expr_filter,
};

pub use self::{
    expr::{CaptureValue, ConditionExpr, Expr, ExprMatch},
    fields::{ExprMatchFilter, Field},
};

pub fn search(database: &mut Database, filters: &[ExprMatchFilter]) -> Vec<ExprMatch> {
    let mut query = database.world.query::<(&Statement, &ValueSources)>();
    let view = query.view();

    let mut matcher = ExprMatcher::new(database, &view);

    database
        .world
        .query::<(&StatementAddr, &Statement, &ValueSources)>()
        .into_iter()
        .filter_map(|(_entity, (addr, stmt, value_sources))| {
            let mut captures = Captures::new();

            for filter in filters {
                let filter_captures =
                    match_expr_filter(&mut matcher, *addr, stmt, value_sources, filter)?;
                captures = captures.union(filter_captures);
            }

            Some(ExprMatch {
                match_addr: *addr,
                captures,
            })
        })
        .collect()
}
