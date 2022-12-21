use std::collections::{HashMap, HashSet};

use hecs::Entity;

use crate::{
    database::{Database, StatementAddr},
    ir::{Statement, Variable},
    utils::vec_set::VecSet,
};

use super::code_flow::{CodeFlowKind, OutCodeFlowEdges};

#[derive(Default)]
pub struct ValueSources(pub HashMap<Variable, VecSet<StatementAddr>>);

fn data_flow_follows_edge_kind(kind: CodeFlowKind) -> bool {
    match kind {
        CodeFlowKind::NextInsn
        | CodeFlowKind::BranchTrue
        | CodeFlowKind::BranchFalse
        | CodeFlowKind::Jump => true,

        CodeFlowKind::Call | CodeFlowKind::Ret => false,
    }
}

fn assigned_var(stmt: &Statement) -> Option<Variable> {
    if let Statement::Assign { lhs, rhs: _ } = stmt {
        Some(*lhs)
    } else {
        None
    }
}

fn get_followed_dst_nodes<'a>(
    db: &'a Database,
    out_edges: &'a OutCodeFlowEdges,
) -> impl Iterator<Item = Entity> + 'a {
    out_edges
        .0
        .iter()
        .filter(|edge| data_flow_follows_edge_kind(edge.kind))
        .filter_map(|edge| edge.to.and_then(|to| db.addr_to_entity.get(&to).copied()))
}

pub fn analyze_data_flow(db: &mut Database) {
    let assigns: Vec<(Entity, Variable, StatementAddr)> = db
        .world
        .query_mut::<(&Statement, &StatementAddr)>()
        .into_iter()
        .filter_map(|(entity, (stmt, addr))| assigned_var(stmt).map(|var| (entity, var, *addr)))
        .collect();

    let mut todo: Vec<Entity> = vec![];
    let mut done: HashSet<Entity> = HashSet::new();

    let mut query = db
        .world
        .query::<(&Statement, &OutCodeFlowEdges, &mut ValueSources)>();
    let mut view = query.view();

    for (assign_entity, var, assign_addr) in assigns {
        // Traverse graph, stopping at any assignment to the same variable. Add
        // this statement as the variable's source for all the traversed nodes.

        todo.clear();
        done.clear();

        for node in get_followed_dst_nodes(db, view.get_mut(assign_entity).unwrap().1) {
            if !done.contains(&node) {
                done.insert(node);
                todo.push(node);
            }
        }

        while let Some(cur) = todo.pop() {
            let (stmt, out_edges, value_sources) = view.get_mut(cur).unwrap();

            // Add the original assignment as the variable's source for the
            // traversed node.
            value_sources.0.entry(var).or_default().insert(assign_addr);

            // Continue the traversal, but stop at any assignment to the same
            // variable.
            if assigned_var(stmt) != Some(var) {
                for node in get_followed_dst_nodes(db, out_edges) {
                    if !done.contains(&node) {
                        done.insert(node);
                        todo.push(node);
                    }
                }
            }
        }
    }
}
