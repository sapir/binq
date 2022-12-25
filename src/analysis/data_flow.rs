use std::collections::{HashMap, HashSet};

use hecs::Entity;
use smallvec::SmallVec;

use crate::{
    database::{Database, StatementAddr},
    ir::{BinaryOp, CompareOp, Expr, SimpleExpr, Statement, UnaryOp, Variable},
    lifting::{REG_CF, REG_DF, REG_IF, REG_OF, REG_SF, REG_ZF},
    utils::small_multi_map::SmallMultiMap,
};

use super::code_flow::{CodeFlowKind, InCodeFlowEdges, OutCodeFlowEdges};

#[derive(Default)]
pub struct ValueSources(pub SmallMultiMap<Variable, StatementAddr>);

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

    let function_starts: HashSet<Entity> = db
        .world
        .query_mut::<&InCodeFlowEdges>()
        .into_iter()
        .filter(|(_entity, in_edges)| {
            in_edges
                .0
                .iter()
                .any(|e| matches!(e.kind, CodeFlowKind::Call))
        })
        .map(|(entity, _)| entity)
        .collect();

    let graph: HashMap<Entity, SmallVec<[Entity; 2]>> = {
        db.world
            .query::<&OutCodeFlowEdges>()
            .into_iter()
            .map(|(entity, out_edges)| {
                (
                    entity,
                    get_followed_dst_nodes(db, out_edges)
                        .filter(|to| !function_starts.contains(to))
                        .collect(),
                )
            })
            .collect()
    };

    let mut todo: UniqueStack<Entity> = UniqueStack::new();

    let mut query = db.world.query::<(&Statement, &mut ValueSources)>();
    let mut view = query.view();

    for (assign_entity, var, assign_addr) in assigns {
        // Traverse graph, stopping at any assignment to the same variable. Add
        // this statement as the variable's source for all the traversed nodes.

        todo.clear();

        for node in graph.get(&assign_entity).into_iter().flatten().copied() {
            todo.push(node);
        }

        while let Some(cur) = todo.pop() {
            let (stmt, value_sources) = view.get_mut(cur).unwrap();

            if matches!(stmt, Statement::ClearTemps) && matches!(var, Variable::Temp(_)) {
                continue;
            }

            // Add the original assignment as the variable's source for the
            // traversed node.
            if uses_var(stmt, var) {
                value_sources.0.insert(var, assign_addr);
            }

            // Continue the traversal, but stop at any assignment to the same
            // variable.
            if assigned_var(stmt) != Some(var) {
                for node in graph.get(&cur).into_iter().flatten().copied() {
                    todo.push(node);
                }
            }
        }
    }
}

fn uses_var(stmt: &Statement, var: Variable) -> bool {
    match stmt {
        Statement::Nop | Statement::ClearTemps => false,
        Statement::Assign { lhs: _, rhs } => expr_uses_var(rhs, var),
        Statement::Store { addr, value } => {
            simple_expr_is_var(addr, var) || simple_expr_is_var(value, var)
        }
        Statement::Call { target } => simple_expr_is_var(target, var),
        Statement::Jump {
            target,
            is_return: _,
            condition,
        } => {
            simple_expr_is_var(target, var)
                || if let Some(expr) = condition {
                    simple_expr_is_var(expr, var)
                } else {
                    false
                }
        }
        Statement::Intrinsic => false,
    }
}

fn expr_uses_var(expr: &Expr, var: Variable) -> bool {
    match expr {
        Expr::Unknown => false,

        Expr::Simple(x) | Expr::Deref(x) | Expr::UnaryOp(UnaryOp { op: _, value: x }) => {
            simple_expr_is_var(x, var)
        }

        Expr::BinaryOp(BinaryOp { op: _, lhs, rhs })
        | Expr::CompareOp(CompareOp { kind: _, lhs, rhs }) => {
            simple_expr_is_var(lhs, var) || simple_expr_is_var(rhs, var)
        }

        Expr::InsertBits {
            lhs,
            shift: _,
            num_bits: _,
            rhs,
        } => simple_expr_is_var(lhs, var) || simple_expr_is_var(rhs, var),

        Expr::X86Flag {
            flag_reg: _,
            from_expr,
        } => *from_expr == var,

        Expr::ComplexX86ConditionCode(_) => match var {
            Variable::Register(reg) => {
                reg == REG_OF
                    || reg == REG_SF
                    || reg == REG_ZF
                    || reg == REG_CF
                    || reg == REG_DF
                    || reg == REG_IF
            }

            Variable::Temp(_) => false,
        },
    }
}

fn simple_expr_is_var(expr: &SimpleExpr, var: Variable) -> bool {
    match expr {
        SimpleExpr::Const(_) => false,
        SimpleExpr::Variable(x) => var == *x,
    }
}

struct UniqueStack<T> {
    set: HashSet<T>,
    stack: Vec<T>,
}

impl<T> UniqueStack<T>
where
    T: Clone + Eq + std::hash::Hash,
{
    pub fn new() -> Self {
        Self {
            set: HashSet::new(),
            stack: vec![],
        }
    }

    pub fn clear(&mut self) {
        self.set.clear();
        self.stack.clear();
    }

    pub fn push(&mut self, value: T) {
        if self.set.insert(value.clone()) {
            self.stack.push(value);
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        self.stack.pop()
    }
}
