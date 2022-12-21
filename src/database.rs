use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Display},
};

use anyhow::Result;
use hecs::{Bundle, Entity, World};

use crate::{
    analysis::{
        code_flow::{InCodeFlowEdges, OutCodeFlowEdges},
        data_flow::ValueSources,
    },
    ir::{Addr64, Statement},
    lifting::X86Lifter,
};

#[derive(Default)]
pub struct Database {
    pub world: World,
    pub addr_to_entity: HashMap<StatementAddr, Entity>,
}

impl Database {
    fn contains_addr(&self, addr: Addr64) -> bool {
        self.addr_to_entity
            .contains_key(&StatementAddr::new_first(addr))
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

            lifter.set_cur_addr(addr);
            let stmts = lifter.lift_block(|stmt_addr| {
                done.contains(&stmt_addr)
                    || self.contains_addr(stmt_addr)
                    || !(base_addr..buf_end_addr).contains(&stmt_addr)
            })?;

            if let Some((_, _, last_stmt)) = stmts.last() {
                match last_stmt {
                    Statement::Jump {
                        target,
                        is_return,
                        condition,
                    } => {
                        // If it's a conditional branch, then continue to the
                        // instruction right after the block, which is also the
                        // lifter's current address right after it decoded the
                        // block.
                        if condition.is_some() {
                            todo.push(lifter.cur_addr());
                        }

                        // Either way, if it's not a return and it has a known
                        // static target...
                        if !*is_return {
                            if let Some(target) = target.as_const() {
                                todo.push(target);
                            }
                        }
                    }

                    _ => {
                        // A block ending with anything else means we either hit
                        // a decoding error, or some instruction that we've
                        // already handled. Either way, don't try to continue to
                        // the next instruction.
                    }
                }
            }

            self.add_block(stmts);

            done.insert(addr);
        }

        Ok(())
    }

    fn add_block(&mut self, stmts: Vec<(StatementAddr, NextStatementAddr, Statement)>) {
        for (addr, next_addr, stmt) in stmts {
            let entity = self.world.spawn(StatementBundle {
                stmt,
                addr,
                next_addr,
                out_code_flow: Default::default(),
                in_code_flow: Default::default(),
                value_sources: Default::default(),
            });

            let old_entity = self.addr_to_entity.insert(addr, entity);

            if let Some(old_entity) = old_entity {
                panic!(
                    "Statement @ {:?} was lifted twice (entities {:?} and {:?})",
                    addr, old_entity, entity
                );
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct StatementAddr {
    /// Address of the original assembly instruction
    pub asm_addr: Addr64,
    /// Index of this statement inside the assembly instruction's IR
    pub ir_index: usize,
}

impl StatementAddr {
    /// A convenience method for creating a `StatementAddr` at the beginning of
    /// an assembly instruction's IL.
    pub fn new_first(asm_addr: Addr64) -> Self {
        Self {
            asm_addr,
            ir_index: 0,
        }
    }
}

impl Display for StatementAddr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#x}/{}", self.asm_addr, self.ir_index)
    }
}

/// The address of the statement directly succeeding this one according to the
/// order of the original file. (It might not actually be the address of a valid
/// statement, though.)
#[derive(Clone, Copy, Debug)]
pub struct NextStatementAddr(pub StatementAddr);

#[derive(Bundle)]
struct StatementBundle {
    stmt: Statement,
    addr: StatementAddr,
    next_addr: NextStatementAddr,
    // Analysis components. We include these at statement spawn time to improve
    // performance.
    out_code_flow: OutCodeFlowEdges,
    in_code_flow: InCodeFlowEdges,
    value_sources: ValueSources,
}
