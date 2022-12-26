use hecs::View;

use crate::{
    analysis::data_flow::ValueSources,
    database::{Database, StatementAddr},
    ir::{BinaryOp, BinaryOpKind, Expr as IrExpr, SimpleExpr, Statement, Variable},
};

#[derive(Clone, Debug)]
pub enum Expr {
    Any,
    Const(u64),
    Deref(Box<Expr>),
    Sum(Vec<Expr>),
    Product(Vec<Expr>),
}

fn get_single_value_source(sources: &ValueSources, var: Variable) -> Option<StatementAddr> {
    let mut var_sources = sources.0.get(&var);
    var_sources
        .next()
        .filter(|_| var_sources.next().is_none())
        .copied()
}

struct ExprMatcher<'db, 'view, 'query> {
    database: &'db Database,
    view: &'view View<'view, (&'query Statement, &'query ValueSources)>,
    visited_addr_stack: Vec<StatementAddr>,
}

impl<'db, 'view, 'query> ExprMatcher<'db, 'view, 'query> {
    fn new(
        database: &'db Database,
        view: &'view View<'view, (&'query Statement, &'query ValueSources)>,
    ) -> Self {
        Self {
            database,
            view,
            visited_addr_stack: vec![],
        }
    }

    fn match_expr(
        &mut self,
        addr: StatementAddr,
        value_sources: &ValueSources,
        pattern_expr: &Expr,
        ir_expr: &IrExpr,
    ) -> bool {
        match pattern_expr {
            Expr::Any => true,

            Expr::Const(_) => {
                if let Some(inner) = self.get_inner_simple_expr(addr, value_sources, ir_expr) {
                    self.match_simple_expr(addr, value_sources, pattern_expr, inner)
                } else {
                    false
                }
            }

            Expr::Deref(pat_inner) => match ir_expr {
                IrExpr::Deref(ir_inner) => {
                    self.match_simple_expr(addr, value_sources, pat_inner, ir_inner)
                }

                _ => false,
            },

            Expr::Sum(inner_patterns) | Expr::Product(inner_patterns) => {
                match inner_patterns.as_slice() {
                    [] => false,

                    [inner_pattern] => self.match_expr(addr, value_sources, inner_pattern, ir_expr),

                    _ => {
                        let expected_op = match pattern_expr {
                            Expr::Sum(_) => BinaryOpKind::Add,
                            Expr::Product(_) => BinaryOpKind::Mul,
                            _ => unreachable!(),
                        };

                        if let IrExpr::BinaryOp(BinaryOp { op, lhs, rhs }) = ir_expr {
                            if *op == expected_op {
                                for (lhs_index, lhs_pattern) in inner_patterns.iter().enumerate() {
                                    if !self.match_simple_expr(
                                        addr,
                                        value_sources,
                                        lhs_pattern,
                                        lhs,
                                    ) {
                                        continue;
                                    };

                                    // TODO: shouldn't have to clone here
                                    let rhs_patterns = {
                                        let mut v = inner_patterns.clone();
                                        v.swap_remove(lhs_index);
                                        v
                                    };

                                    let rhs_pattern = match pattern_expr {
                                        Expr::Sum(_) => Expr::Sum(rhs_patterns),
                                        Expr::Product(_) => Expr::Product(rhs_patterns),
                                        _ => unreachable!(),
                                    };

                                    if self.match_simple_expr(
                                        addr,
                                        value_sources,
                                        &rhs_pattern,
                                        rhs,
                                    ) {
                                        return true;
                                    }
                                }
                            }
                        }

                        false
                    }
                }
            }
        }
    }

    fn match_simple_expr(
        &mut self,
        addr: StatementAddr,
        value_sources: &ValueSources,
        pattern_expr: &Expr,
        ir_expr: &SimpleExpr,
    ) -> bool {
        match (pattern_expr, ir_expr) {
            (Expr::Any, _) => true,

            (_, SimpleExpr::Variable(ir_var)) => {
                self.match_var(addr, value_sources, pattern_expr, *ir_var)
            }

            (Expr::Const(pat_x), SimpleExpr::Const(ir_x)) => *pat_x == *ir_x,

            (Expr::Deref(_), _) => false,

            (Expr::Sum(terms), ir_expr) => {
                if let [term] = &terms[..] {
                    self.match_simple_expr(addr, value_sources, term, ir_expr)
                } else {
                    false
                }
            }

            (Expr::Product(factors), ir_expr) => {
                if let [factor] = &factors[..] {
                    self.match_simple_expr(addr, value_sources, factor, ir_expr)
                } else {
                    false
                }
            }
        }
    }

    fn match_var(
        &mut self,
        addr: StatementAddr,
        value_sources: &ValueSources,
        pattern_expr: &Expr,
        ir_var: Variable,
    ) -> bool {
        let Some(source_addr) = get_single_value_source(value_sources, ir_var)
        else { return false };

        if self.visited_addr_stack.contains(&source_addr) {
            // Loop!
            return false;
        }

        let entity = self.database.addr_to_entity[&source_addr];
        let (source_stmt, source_value_sources) = self.view.get(entity).unwrap();
        let Statement::Assign { lhs, rhs } = source_stmt
        else { panic!("source isn't an Assign statement") };
        debug_assert_eq!(*lhs, ir_var);

        self.visited_addr_stack.push(source_addr);
        let result = self.match_expr(addr, source_value_sources, pattern_expr, rhs);
        assert_eq!(self.visited_addr_stack.pop(), Some(source_addr));
        result
    }

    fn get_inner_simple_expr<'a>(
        &mut self,
        addr: StatementAddr,
        value_sources: &ValueSources,
        expr: &'a IrExpr,
    ) -> Option<&'a SimpleExpr> {
        match expr {
            IrExpr::Unknown => None,

            IrExpr::Simple(inner) => Some(inner),

            IrExpr::BinaryOp(BinaryOp { op, lhs, rhs }) => {
                let identity = match op {
                    BinaryOpKind::Add
                    | BinaryOpKind::Sub
                    | BinaryOpKind::Shl
                    | BinaryOpKind::Shr
                    | BinaryOpKind::Sar
                    | BinaryOpKind::Rol
                    | BinaryOpKind::Ror
                    | BinaryOpKind::Or
                    | BinaryOpKind::Xor => 0,

                    BinaryOpKind::Mul => 1,

                    BinaryOpKind::And => {
                        // TODO: -1, but it depends on the data size
                        return None;
                    }
                };

                let is_commutative = match op {
                    BinaryOpKind::Add
                    | BinaryOpKind::Mul
                    | BinaryOpKind::And
                    | BinaryOpKind::Or
                    | BinaryOpKind::Xor => true,

                    BinaryOpKind::Sub
                    | BinaryOpKind::Shl
                    | BinaryOpKind::Shr
                    | BinaryOpKind::Sar
                    | BinaryOpKind::Rol
                    | BinaryOpKind::Ror => false,
                };

                let const_pattern = Expr::Const(identity);
                if self.match_simple_expr(addr, value_sources, &const_pattern, rhs) {
                    Some(lhs)
                } else if is_commutative
                    && self.match_simple_expr(addr, value_sources, &const_pattern, lhs)
                {
                    Some(rhs)
                } else {
                    None
                }
            }

            IrExpr::Deref(_)
            | IrExpr::UnaryOp(_)
            | IrExpr::CompareOp(_)
            | IrExpr::InsertBits { .. }
            | IrExpr::X86Flag { .. }
            | IrExpr::ComplexX86ConditionCode(_) => None,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Field {
    Value,
    StoreAddr,
    Target,
    Condition,
}

#[derive(Clone, Debug)]
pub struct ExprMatchFilter {
    pub field: Field,
    pub expr: Expr,
}

enum MaybeSimpleExpr<'a> {
    Simple(&'a SimpleExpr),
    Complex(&'a IrExpr),
}

fn get_field(stmt: &Statement, field: Field) -> Option<MaybeSimpleExpr> {
    match (stmt, field) {
        (Statement::Nop, _) => None,

        (Statement::Assign { rhs, .. }, Field::Value) => Some(MaybeSimpleExpr::Complex(rhs)),
        (Statement::Assign { .. }, _) => None,

        (Statement::Store { addr, .. }, Field::StoreAddr) => Some(MaybeSimpleExpr::Simple(addr)),
        (Statement::Store { value, .. }, Field::Value) => Some(MaybeSimpleExpr::Simple(value)),
        (Statement::Store { .. }, _) => None,

        (Statement::Call { target } | Statement::Jump { target, .. }, Field::Target) => {
            Some(MaybeSimpleExpr::Simple(target))
        }

        (
            Statement::Jump {
                condition: Some(condition),
                ..
            },
            Field::Condition,
        ) => Some(MaybeSimpleExpr::Simple(condition)),

        (Statement::Call { .. } | Statement::Jump { .. }, _) => None,

        (Statement::Intrinsic | Statement::ClearTemps, _) => None,
    }
}

fn match_expr_filter(
    matcher: &mut ExprMatcher,
    addr: StatementAddr,
    stmt: &Statement,
    value_sources: &ValueSources,
    filter: &ExprMatchFilter,
) -> bool {
    let ExprMatchFilter { field, expr } = filter;

    let Some(ir_expr) = get_field(stmt, *field) else { return false };

    match ir_expr {
        MaybeSimpleExpr::Simple(ir_expr) => {
            matcher.match_simple_expr(addr, value_sources, expr, ir_expr)
        }

        MaybeSimpleExpr::Complex(ir_expr) => matcher.match_expr(addr, value_sources, expr, ir_expr),
    }
}

pub fn search(database: &mut Database, filters: &[ExprMatchFilter]) {
    let mut query = database.world.query::<(&Statement, &ValueSources)>();
    let view = query.view();

    let mut matcher = ExprMatcher::new(database, &view);

    for (_entity, (addr, stmt, value_sources)) in database
        .world
        .query::<(&StatementAddr, &Statement, &ValueSources)>()
        .into_iter()
    {
        if filters
            .iter()
            .all(|filter| match_expr_filter(&mut matcher, *addr, stmt, value_sources, filter))
        {
            println!("{}", *addr);
        }
    }
}
