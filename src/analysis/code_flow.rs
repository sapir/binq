use crate::{
    database::{Database, NextStatementAddr, StatementAddr},
    ir::Statement,
    utils::insert_default_bundles,
};

#[derive(Clone, Copy, Debug)]
pub enum CodeFlowKind {
    NextInsn,
    Jump,
    BranchTrue,
    BranchFalse,
    Call,
    Ret,
}

#[derive(Clone, Debug)]
pub struct CodeFlowEdge {
    pub kind: CodeFlowKind,
    pub from: StatementAddr,
    /// Only `Some` if target is statically known.
    pub to: Option<StatementAddr>,
    pub is_conditional: bool,
}

#[derive(Default)]
pub struct InCodeFlowEdges(pub Vec<CodeFlowEdge>);

#[derive(Default)]
pub struct OutCodeFlowEdges(pub Vec<CodeFlowEdge>);

pub fn add_code_flow(db: &mut Database) {
    insert_default_bundles::<(InCodeFlowEdges, OutCodeFlowEdges), &Statement>(&mut db.world);

    let mut in_query = db.world.query::<&mut InCodeFlowEdges>();
    let mut in_view = in_query.view();

    for (_, (stmt, addr, next_addr, out)) in db
        .world
        .query::<(
            &Statement,
            &StatementAddr,
            &NextStatementAddr,
            &mut OutCodeFlowEdges,
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
}
