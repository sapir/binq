use std::collections::HashMap;

use hecs::{Bundle, Entity, World};
use itertools::Itertools;

use crate::ir::{Addr64, Block, JumpKind, Statement};

#[derive(Default)]
pub struct Database {
    pub world: World,
    pub addr_to_entity: HashMap<Addr64, Entity>,
}

impl Database {
    pub fn add_block(&mut self, block: Block) {
        let Block {
            statements,
            next,
            jump_kind,
        } = block;

        if statements.is_empty() && jump_kind == JumpKind::NoDecode {
            // Don't bother, it's an invalid block.
            return;
        }

        let mut addr = None;

        let entities = statements
            .into_iter()
            .chain(std::iter::once(Statement::EndOfBlock { next, jump_kind }))
            .map(|stmt| {
                if let Statement::IMark {
                    addr: orig_addr, ..
                } = stmt
                {
                    addr = Some(StatementAddr {
                        orig_addr,
                        ir_stmt_offset: 0,
                    });
                } else {
                    addr.as_mut().expect("missing IMark").ir_stmt_offset += 1;
                }

                self.world.spawn(StatementBundle {
                    stmt,
                    links: IntraBlockLinks::default(),
                    addr: addr.expect("missing IMark"),
                })
            })
            .collect::<Vec<_>>();

        // Preserve the order in the block by linking the statements to each other.
        let mut links_query = self.world.query::<&mut IntraBlockLinks>();
        let mut links_view = links_query.view();
        for (a, b) in entities.into_iter().tuple_windows() {
            let [a_links, b_links] = links_view.get_mut_n([a, b]);
            let a_links = a_links.unwrap();
            let b_links = b_links.unwrap();
            a_links.next = Some(b);
            b_links.prev = Some(a);
        }
    }
}

#[derive(Default)]
pub struct IntraBlockLinks {
    pub prev: Option<Entity>,
    pub next: Option<Entity>,
}

#[derive(Clone, Copy, Debug)]
pub struct StatementAddr {
    /// Address at the last IMark
    pub orig_addr: Addr64,
    /// Number of statements since the last IMark
    pub ir_stmt_offset: usize,
}

#[derive(Bundle)]
struct StatementBundle {
    stmt: Statement,
    links: IntraBlockLinks,
    addr: StatementAddr,
}
