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

pub fn search(database: &mut Database, filters: &[ExprMatchFilter]) {
    let mut query = database.world.query::<(&Statement, &ValueSources)>();
    let view = query.view();

    let mut matcher = ExprMatcher::new(database, &view);

    for (_entity, (addr, stmt, value_sources)) in database
        .world
        .query::<(&StatementAddr, &Statement, &ValueSources)>()
        .into_iter()
    {
        if filters
            .iter()
            .all(|filter| match_expr_filter(&mut matcher, *addr, stmt, value_sources, filter))
        {
            println!("{}", *addr);
        }
    }
}
