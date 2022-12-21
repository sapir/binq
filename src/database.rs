use std::collections::{HashMap, HashSet};

use anyhow::Result;
use hecs::{Bundle, Entity, World};
use itertools::Itertools;

use crate::{
    analysis::code_flow::{InCodeFlowEdges, OutCodeFlowEdges},
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
                let entity = self.world.spawn(StatementBundle {
                    stmt,
                    addr,
                    links: Default::default(),
                    out_code_flow: Default::default(),
                    in_code_flow: Default::default(),
                });

                if let StatementAddr {
                    asm_addr,
                    ir_index: 0,
                } = addr
                {
                    let old_entity = self.addr_to_entity.insert(asm_addr, entity);

                    if let Some(old_entity) = old_entity {
                        panic!(
                            "Statement @ {:#x} was lifted twice (entities {:?} and {:?})",
                            asm_addr, old_entity, entity
                        );
                    }
                }

                entity
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
    /// Address of the original assembly instruction
    pub asm_addr: Addr64,
    /// Index of this statement inside the assembly instruction's IR
    pub ir_index: usize,
}

#[derive(Bundle)]
struct StatementBundle {
    stmt: Statement,
    addr: StatementAddr,
    links: IntraBlockLinks,
    // Analysis components. We include these at statement spawn time to improve
    // performance.
    out_code_flow: OutCodeFlowEdges,
    in_code_flow: InCodeFlowEdges,
}
