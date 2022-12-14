use hecs::Entity;

use crate::{
    database::{Database, IntraBlockLinks},
    ir::{Addr64, Expr, JumpKind, Statement},
    utils::insert_default_bundles,
};

#[derive(Clone, Copy, Debug)]
pub enum CodeFlowKind {
    NextInsn,
    Jump,
    Call,
    Ret,
    Special,
}

impl CodeFlowKind {
    pub(crate) fn for_jump_kind(jk: JumpKind) -> Option<Self> {
        Some(match jk {
            JumpKind::Boring => Self::Jump,
            JumpKind::Call => Self::Call,
            JumpKind::Ret => Self::Ret,

            JumpKind::NoDecode => {
                return None;
            }

            JumpKind::Yield
            | JumpKind::InvalICache
            | JumpKind::FlushDCache
            | JumpKind::SigILL
            | JumpKind::SigTRAP
            | JumpKind::SigSEGV
            | JumpKind::SigBUS
            | JumpKind::SigFPE
            | JumpKind::SigFPE_IntDiv
            | JumpKind::SigFPE_IntOvf
            | JumpKind::Privileged
            | JumpKind::Sys_syscall
            | JumpKind::Sys_int32
            | JumpKind::Sys_int128
            | JumpKind::Sys_int129
            | JumpKind::Sys_int130
            | JumpKind::Sys_int145
            | JumpKind::Sys_int210
            | JumpKind::Sys_sysenter => Self::Special,

            _ => todo!("jump kind {:?}", jk),
        })
    }
}

#[derive(Clone, Debug)]
pub struct CodeFlowEdge {
    pub kind: CodeFlowKind,
    pub from: Entity,
    /// Only `Some` if target is statically known and exists in the database.
    pub to: Option<Entity>,
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

    let addr_to_entity = {
        let addr_to_entity = &db.addr_to_entity;
        move |addr: Addr64| addr_to_entity.get(&addr).copied()
    };

    for (entity, (stmt, links, out)) in db
        .world
        .query::<(&Statement, &IntraBlockLinks, &mut OutCodeFlowEdges)>()
        .into_iter()
    {
        let out = &mut out.0;

        let mut kind_of_edge_to_next = None;

        // TODO: calls inside expressions
        match stmt {
            Statement::NoOp
            | Statement::IMark { .. }
            | Statement::AbiHint
            | Statement::Put { .. }
            | Statement::PutI
            | Statement::WrTmp { .. }
            | Statement::Store { .. }
            | Statement::CAS
            | Statement::LLSC
            | Statement::MemoryBusEvent
            | Statement::LoadG
            | Statement::StoreG => {
                kind_of_edge_to_next = Some(CodeFlowKind::NextInsn);
            }

            Statement::Dirty => {
                out.push(CodeFlowEdge {
                    kind: CodeFlowKind::Call,
                    from: entity,
                    to: None,              // TODO
                    is_conditional: false, // TODO: can be conditional!
                });
            }

            // Conditional jumps
            Statement::Exit { dst, jump_kind, .. } => {
                let addr = u64::try_from(*dst).unwrap();

                out.push(CodeFlowEdge {
                    kind: CodeFlowKind::for_jump_kind(*jump_kind).unwrap(),
                    from: entity,
                    to: addr_to_entity(addr),
                    is_conditional: true,
                });
            }

            Statement::EndOfBlock { next, jump_kind } => match (next, *jump_kind) {
                (Expr::Const(_), JumpKind::NoDecode) => {
                    // No next instruction
                }

                (Expr::Const(next), jk) => {
                    let kind = CodeFlowKind::for_jump_kind(jk).unwrap();
                    out.push(CodeFlowEdge {
                        kind,
                        from: entity,
                        to: addr_to_entity((*next).try_into().unwrap()),
                        is_conditional: false,
                    });
                }

                (Expr::RdTmp(_) | Expr::Get { .. } | Expr::GetI { .. }, jk) => {
                    // Dynamic jump
                    let kind = CodeFlowKind::for_jump_kind(jk).unwrap();
                    out.push(CodeFlowEdge {
                        kind,
                        from: entity,
                        to: None,
                        is_conditional: false,
                    });
                }

                _ => todo!("code flow for (next={:?}, jump_kind={:?})", next, jump_kind),
            },
        }

        if let Some(kind) = kind_of_edge_to_next {
            debug_assert!(!matches!(stmt, Statement::EndOfBlock { .. }));

            let is_conditional = !out.is_empty();

            out.push(CodeFlowEdge {
                kind,
                from: entity,
                to: Some(
                    links
                        .next
                        .expect("missing next link for statement not at the end of a block"),
                ),
                is_conditional,
            });
        }

        // Add the reverse edges
        for edge in out {
            if let Some(to) = edge.to {
                in_view.get_mut(to).unwrap().0.push(edge.clone());
            }
        }
    }
}
