use std::collections::{HashMap, HashSet};

use anyhow::Result;
use hecs::{Bundle, Entity, World};
use itertools::Itertools;

use crate::{
    ir::{Addr64, SimpleExpr, Statement},
    lifting::X86Lifter,
};

#[derive(Default)]
pub struct Database {
    pub world: World,
    pub addr_to_entity: HashMap<Addr64, Entity>,
}

impl Database {
    fn contains_addr(&self, addr: Addr64) -> bool {
        self.addr_to_entity.contains_key(&addr)
    }

    pub fn add_func(&mut self, base_addr: Addr64, buf: &[u8], start_addr: Addr64) -> Result<()> {
        let mut lifter = X86Lifter::new(buf, base_addr);
        let buf_end_addr = base_addr + Addr64::try_from(buf.len()).unwrap();

        let mut todo = vec![start_addr];
        let mut done: HashSet<Addr64> = HashSet::new();

        while let Some(addr) = todo.pop() {
            // If already handled in this function, or previously inserted by
            // another function, or if the address is simply out of range.
            if done.contains(&addr)
                || self.contains_addr(addr)
                || !(base_addr..buf_end_addr).contains(&addr)
            {
                continue;
            }

            done.insert(addr);

            lifter.set_cur_addr(addr);
            let stmts = lifter.lift_block()?;

            if let Some((_, last_stmt)) = stmts.last() {
                match last_stmt {
                    Statement::Jump {
                        is_return: true, ..
                    } => {
                        // Don't continue past a return statement.
                    }

                    Statement::Jump {
                        target,
                        is_return: false,
                        condition,
                    } => {
                        // It's a conditional branch. Continue to the
                        // instruction right after the block, which is also the
                        // lifter's current address right after it decoded the
                        // block.
                        if condition.is_some() {
                            todo.push(lifter.cur_addr());
                        }

                        // Either way, if it's a known static target...
                        if let SimpleExpr::Const(target) = target {
                            todo.push(*target);
                        }
                    }

                    _ => {
                        // A block ending with anything else means we hit a
                        // decoding error. Don't try to continue to the next
                        // instruction.
                    }
                }
            }

            self.add_block(stmts);
        }

        Ok(())
    }

    fn add_block(&mut self, stmts: Vec<(StatementAddr, Statement)>) {
        let entities = stmts
            .into_iter()
            .map(|(addr, stmt)| {
                self.world.spawn(StatementBundle {
                    stmt,
                    links: IntraBlockLinks::default(),
                    addr,
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

    pub fn reindex(&mut self) {
        // TODO
        /*
        self.addr_to_entity = self
            .world
            .query_mut::<&Statement>()
            .into_iter()
            .filter_map(|(entity, stmt)| match stmt {
                Statement::IMark { addr, .. } => Some((*addr, entity)),
                _ => None,
            })
            .collect();
            */
    }
}

#[derive(Default)]
pub struct IntraBlockLinks {
    pub prev: Option<Entity>,
    pub next: Option<Entity>,
}

#[derive(Clone, Copy, Debug)]
pub struct StatementAddr {
    /// Address of the original assembly instruction
    pub asm_addr: Addr64,
    /// Index of this statement inside the assembly instruction's IR
    pub ir_index: usize,
}

#[derive(Bundle)]
struct StatementBundle {
    stmt: Statement,
    links: IntraBlockLinks,
    addr: StatementAddr,
}
