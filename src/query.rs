mod expr;
mod fields;

use crate::{
    analysis::data_flow::ValueSources,
    database::{Database, StatementAddr},
    ir::{Expr as IrExpr, Statement},
};

use self::{
    expr::{Captures, ExprMatcher},
    fields::match_expr_filter,
};

pub use self::{
    expr::{BranchMatch, BranchMatchData, CaptureValue, ConditionExpr, Expr, ExprMatch},
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

pub fn search_branch(database: &mut Database, pattern: &ConditionExpr) -> Vec<BranchMatch> {
    let mut query = database.world.query::<(&Statement, &ValueSources)>();
    let view = query.view();

    let mut matcher = ExprMatcher::new(database, &view);

    database
        .world
        .query::<(&StatementAddr, &Statement, &ValueSources)>()
        .into_iter()
        .filter_map(|(_entity, (addr, stmt, value_sources))| {
            let Statement::Jump { condition, .. } = stmt
            else { return None };

            let condition = *condition.as_ref()?;

            let branch_match = matcher.match_condition_pattern(
                *addr,
                value_sources,
                pattern,
                &IrExpr::Simple(condition),
            )?;

            Some(BranchMatch {
                match_addr: *addr,
                data: branch_match,
            })
        })
        .collect()
}
