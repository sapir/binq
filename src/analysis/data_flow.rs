use hecs::{Entity, World};

use crate::{
    database::{Database, StatementAddr},
    ir::{
        BinaryOp, CompareOp, Expr, ExtendOp, SimpleExpr, Statement, UnaryOp, Variable,
        X86FlagResult,
    },
    lifting::{REG_CF, REG_OF, REG_SF, REG_ZF},
    utils::small_multi_map::SmallMultiMap,
};

use super::code_flow::{CodeFlowKind, InCodeFlowEdges, OutCodeFlowEdges};

#[derive(Default, Debug)]
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

struct IsFunctionStart(bool);

/// Used to mark nodes reached in the current graph traversal.
struct Tag(u64);

struct FollowedOutEdges(Vec<Entity>);

pub fn analyze_data_flow(db: &mut Database) {
    let assigns: Vec<(Entity, Variable, StatementAddr)> = db
        .world
        .query_mut::<(&Statement, &StatementAddr)>()
        .into_iter()
        .filter_map(|(entity, (stmt, addr))| assigned_var(stmt).map(|var| (entity, var, *addr)))
        .collect();

    // Build storage for some temporary components in a separate hecs World, but
    // using the same entities.
    let mut tmp_world = World::new();

    // Spawn components, filling in IsFunctionStart.
    for (entity, in_edges) in db.world.query_mut::<&InCodeFlowEdges<Entity>>() {
        let is_function_start = in_edges
            .0
            .iter()
            .any(|e| matches!(e.kind, CodeFlowKind::Call));

        tmp_world.spawn_at(
            entity,
            (
                Tag(0),
                FollowedOutEdges(vec![]),
                IsFunctionStart(is_function_start),
            ),
        );
    }

    // Fill in FollowedOutEdges. This is a separate step because it uses the
    // value of InFunctionStart from the previous iteration.
    {
        let mut edges_query = tmp_world.query::<&mut FollowedOutEdges>();
        let mut edges_view = edges_query.view();

        let mut func_start_query = tmp_world.query::<&IsFunctionStart>();
        let mut func_start_view = func_start_query.view();

        for (entity, out_edges) in db.world.query_mut::<&OutCodeFlowEdges<Entity>>() {
            let followed_out_edges = edges_view.get_mut(entity).unwrap();
            followed_out_edges.0 = out_edges
                .0
                .iter()
                .filter(|edge| data_flow_follows_edge_kind(edge.kind))
                .filter_map(|edge| edge.to)
                .filter(|to| !func_start_view.get_mut(*to).unwrap().0)
                .collect();
        }
    }

    let mut cur_tag = 0;

    let mut todo: Vec<Entity> = vec![];

    let mut query = db.world.query::<(&Statement, &mut ValueSources)>();
    let mut view = query.view();

    let mut tmp_query = tmp_world.query_mut::<(&FollowedOutEdges, &mut Tag)>();
    let mut tmp_view = tmp_query.view();

    for (assign_entity, var, assign_addr) in assigns {
        // Traverse graph, stopping at any assignment to the same variable. Add
        // this statement as the variable's source for all the traversed nodes.

        cur_tag += 1;

        todo.clear();
        todo.extend_from_slice(&tmp_view.get_mut(assign_entity).unwrap().0 .0);

        while let Some(cur) = todo.pop() {
            let (stmt, value_sources) = view.get_mut(cur).unwrap();
            let (followed_out_edges, tag) = tmp_view.get_mut(cur).unwrap();

            if tag.0 == cur_tag {
                continue;
            }

            tag.0 = cur_tag;

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
                todo.extend_from_slice(&followed_out_edges.0);
            }
        }
    }
}

/// Returns true if the variable is referenced by the statement and should
/// therefore be tracked.
fn uses_var(stmt: &Statement, var: Variable) -> bool {
    match stmt {
        Statement::Nop | Statement::ClearTemps => false,
        Statement::Assign { lhs: _, rhs } => expr_uses_var(rhs, var),
        Statement::Store {
            addr,
            value,
            size_bytes: _,
        } => simple_expr_is_var(addr, var) || simple_expr_is_var(value, var),
        Statement::Call { target } => {
            // Registers can be used as arguments, but don't bother checking the
            // ABI, instead just track all registers for calls.
            matches!(var, Variable::Register(_)) || simple_expr_is_var(target, var)
        }
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

        Expr::Simple(x)
        | Expr::Extend(ExtendOp { kind: _, inner: x })
        | Expr::Deref {
            ptr: x,
            size_bytes: _,
        }
        | Expr::UnaryOp(UnaryOp { op: _, value: x }) => simple_expr_is_var(x, var),

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

        Expr::X86Flag(X86FlagResult {
            which_flag: _,
            mnemonic: _,
            lhs,
            rhs,
            math_result,
        }) => simple_expr_is_var(lhs, var) || simple_expr_is_var(rhs, var) || *math_result == var,

        Expr::ComplexX86ConditionCode(_) => match var {
            Variable::Register(reg) => {
                reg == REG_OF || reg == REG_SF || reg == REG_ZF || reg == REG_CF
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
