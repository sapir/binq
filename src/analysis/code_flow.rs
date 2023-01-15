use std::collections::HashMap;

use hecs::Entity;

use crate::{
    database::{Database, NextStatementAddr, StatementAddr},
    ir::Statement,
    utils::insert_default_bundles,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CodeFlowKind {
    NextInsn,
    Jump,
    BranchTrue,
    BranchFalse,
    Call,
    Ret,
}

#[derive(Clone, Debug)]
pub struct CodeFlowEdge<AddrType> {
    pub kind: CodeFlowKind,
    pub from: AddrType,
    /// Only `Some` if target is statically known.
    pub to: Option<AddrType>,
    pub is_conditional: bool,
}

#[derive(Debug)]
pub struct InCodeFlowEdges<AddrType>(pub Vec<CodeFlowEdge<AddrType>>);

impl<AddrType> Default for InCodeFlowEdges<AddrType> {
    fn default() -> Self {
        Self(Default::default())
    }
}

#[derive(Debug)]
pub struct OutCodeFlowEdges<AddrType>(pub Vec<CodeFlowEdge<AddrType>>);

impl<AddrType> Default for OutCodeFlowEdges<AddrType> {
    fn default() -> Self {
        Self(Default::default())
    }
}

pub fn add_code_flow(db: &mut Database) {
    insert_default_bundles::<
        (
            InCodeFlowEdges<StatementAddr>,
            OutCodeFlowEdges<StatementAddr>,
            InCodeFlowEdges<Entity>,
            OutCodeFlowEdges<Entity>,
        ),
        &Statement,
    >(&mut db.world);

    let mut in_query = db.world.query::<&mut InCodeFlowEdges<StatementAddr>>();
    let mut in_view = in_query.view();

    for (_, (stmt, addr, next_addr, out)) in db
        .world
        .query::<(
            &Statement,
            &StatementAddr,
            &NextStatementAddr,
            &mut OutCodeFlowEdges<StatementAddr>,
        )>()
        .into_iter()
    {
        let out = &mut out.0;

        let kind_of_edge_to_next;

        match stmt {
            Statement::Nop
            | Statement::ClearTemps
            | Statement::Assign { .. }
            | Statement::Store { .. }
            | Statement::Intrinsic => {
                kind_of_edge_to_next = Some(CodeFlowKind::NextInsn);
            }

            Statement::Call { target } => {
                out.push(CodeFlowEdge {
                    kind: CodeFlowKind::Call,
                    from: *addr,
                    to: target.as_const().map(StatementAddr::new_first),
                    is_conditional: false,
                });

                kind_of_edge_to_next = Some(CodeFlowKind::NextInsn);
            }

            Statement::Jump {
                target,
                is_return,
                condition,
            } => {
                let kind = if *is_return {
                    CodeFlowKind::Ret
                } else if condition.is_some() {
                    CodeFlowKind::BranchTrue
                } else {
                    CodeFlowKind::Jump
                };

                out.push(CodeFlowEdge {
                    kind,
                    from: *addr,
                    to: target.as_const().map(StatementAddr::new_first),
                    is_conditional: condition.is_some(),
                });

                kind_of_edge_to_next = if condition.is_some() {
                    Some(CodeFlowKind::BranchFalse)
                } else {
                    None
                };
            }
        }

        if let Some(kind) = kind_of_edge_to_next {
            let is_conditional = !out.is_empty();

            out.push(CodeFlowEdge {
                kind,
                from: *addr,
                to: Some(next_addr.0),
                is_conditional,
            });
        }

        // Add the reverse edges
        for edge in out {
            if let Some(to) = edge.to {
                if let Some(to) = db.addr_to_entity.get(&to) {
                    in_view.get_mut(*to).unwrap().0.push(edge.clone());
                }
            }
        }
    }

    drop(in_query);

    // Map StatementAddr edges to Entity edges

    for (_, (out_edges_by_addr, out_edges_by_entity)) in db.world.query_mut::<(
        &OutCodeFlowEdges<StatementAddr>,
        &mut OutCodeFlowEdges<Entity>,
    )>() {
        out_edges_by_entity.0.clear();
        out_edges_by_entity.0.extend(
            out_edges_by_addr
                .0
                .iter()
                .filter_map(|edge| map_edge(&db.addr_to_entity, edge)),
        );
    }

    for (_, (in_edges_by_addr, in_edges_by_entity)) in db.world.query_mut::<(
        &InCodeFlowEdges<StatementAddr>,
        &mut InCodeFlowEdges<Entity>,
    )>() {
        in_edges_by_entity.0.clear();
        in_edges_by_entity.0.extend(
            in_edges_by_addr
                .0
                .iter()
                .filter_map(|edge| map_edge(&db.addr_to_entity, edge)),
        );
    }
}

fn map_edge(
    addr_to_entity: &HashMap<StatementAddr, Entity>,
    edge: &CodeFlowEdge<StatementAddr>,
) -> Option<CodeFlowEdge<Entity>> {
    Some(CodeFlowEdge {
        kind: edge.kind,
        from: addr_to_entity.get(&edge.from).copied()?,
        to: edge.to.and_then(|to| addr_to_entity.get(&to).copied()),
        is_conditional: edge.is_conditional,
    })
}
