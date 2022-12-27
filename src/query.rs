mod expr;
mod fields;

use crate::{
    analysis::data_flow::ValueSources,
    database::{Database, StatementAddr},
    ir::Statement,
};

use self::{expr::ExprMatcher, fields::match_expr_filter};

pub use self::{
    expr::Expr,
    fields::{ExprMatchFilter, Field},
};

pub fn search(database: &mut Database, filters: &[ExprMatchFilter]) -> Vec<StatementAddr> {
    let mut query = database.world.query::<(&Statement, &ValueSources)>();
    let view = query.view();

    let mut matcher = ExprMatcher::new(database, &view);

    database
        .world
        .query::<(&StatementAddr, &Statement, &ValueSources)>()
        .into_iter()
        .filter_map(|(_entity, (addr, stmt, value_sources))| {
            if filters
                .iter()
                .all(|filter| match_expr_filter(&mut matcher, *addr, stmt, value_sources, filter))
            {
                Some(*addr)
            } else {
                None
            }
        })
        .collect()
}
