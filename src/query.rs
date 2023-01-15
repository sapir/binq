mod expr;
mod fields;

use crate::{
    analysis::{
        code_flow::{CodeFlowKind, OutCodeFlowEdges},
        data_flow::ValueSources,
    },
    database::{Database, StatementAddr},
    ir::{Expr as IrExpr, Statement},
};

use self::{
    expr::{BranchMatchData, ExprMatcher},
    fields::match_expr_filter,
};

pub use self::{
    expr::{BranchMatch, BranchMatchKind, CaptureValue, Captures, ConditionExpr, Expr, ExprMatch},
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
        .query::<(
            &StatementAddr,
            &Statement,
            &ValueSources,
            &OutCodeFlowEdges<StatementAddr>,
        )>()
        .into_iter()
        .filter_map(|(_entity, (addr, stmt, value_sources, out_edges))| {
            let Statement::Jump { condition, .. } = stmt
            else { return None };

            let condition = *condition.as_ref()?;

            let branch_match = matcher.match_condition_pattern(
                *addr,
                value_sources,
                pattern,
                &IrExpr::Simple(condition),
            )?;

            let BranchMatchData {
                captures,
                branch_match_kind,
            } = branch_match;

            let mut true_addr = out_edges
                .0
                .iter()
                .find(|edge| edge.kind == CodeFlowKind::BranchTrue)
                .and_then(|edge| edge.to);
            let mut false_addr = out_edges
                .0
                .iter()
                .find(|edge| edge.kind == CodeFlowKind::BranchFalse)
                .and_then(|edge| edge.to);

            // JumpIfFalse means the pattern condition is inverted relative to
            // the branch condition, so the branch's true is the pattern's false
            // and vice versa.
            if branch_match_kind == BranchMatchKind::JumpIfFalse {
                std::mem::swap(&mut true_addr, &mut false_addr);
            }

            Some(BranchMatch {
                match_addr: *addr,
                captures,
                branch_match_kind,
                true_addr,
                false_addr,
            })
        })
        .collect()
}
