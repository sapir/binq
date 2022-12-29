use hecs::View;
use iced_x86::Mnemonic;

use crate::{
    analysis::data_flow::ValueSources,
    database::{Database, StatementAddr},
    ir::{
        BinaryOp, BinaryOpKind, CompareOp, CompareOpKind, ComplexX86ConditionCode, Expr as IrExpr,
        SimpleExpr, Statement, UnaryOp, UnaryOpKind, Variable, X86Flag, X86FlagResult,
    },
    lifting::ir_flag_to_register,
};

#[derive(Clone, Debug)]
pub enum Expr {
    Any,
    AnyConst,
    Const(u64),
    Deref(Box<Expr>),
    Sum(Vec<Expr>),
    Product(Vec<Expr>),
    Condition(Box<ConditionExpr>),
}

#[derive(Clone, Debug)]
pub struct ConditionExpr {
    pub kind: CompareOpKind,
    pub lhs: Expr,
    pub rhs: Expr,
}

impl ConditionExpr {
    pub fn inverse(&self) -> Self {
        // TODO: don't clone
        Self {
            kind: self.kind.swapped_inverse(),
            lhs: self.rhs.clone(),
            rhs: self.lhs.clone(),
        }
    }
}

// TODO: multiple sources are OK as long as they all expand to the same thing
// (either a constant or a specific variable+location)
fn get_single_value_source(sources: &ValueSources, var: Variable) -> Option<StatementAddr> {
    let mut var_sources = sources.0.get(&var);
    var_sources
        .next()
        .filter(|_| var_sources.next().is_none())
        .copied()
}

struct ExprMatcherAt<'db, 'view, 'query, 'a> {
    addr: StatementAddr,
    value_sources: &'a ValueSources,
    matcher: &'a mut ExprMatcher<'db, 'view, 'query>,
}

impl<'db, 'view, 'query, 'a> ExprMatcherAt<'db, 'view, 'query, 'a> {
    fn new(
        addr: StatementAddr,
        value_sources: &'a ValueSources,
        matcher: &'a mut ExprMatcher<'db, 'view, 'query>,
    ) -> Self {
        matcher.visited_addr_stack.push(addr);
        Self {
            addr,
            value_sources,
            matcher,
        }
    }
}

impl Drop for ExprMatcherAt<'_, '_, '_, '_> {
    fn drop(&mut self) {
        assert_eq!(self.matcher.visited_addr_stack.pop(), Some(self.addr));
    }
}

impl<'db, 'view, 'query, 'a> ExprMatcherAt<'db, 'view, 'query, 'a> {
    fn match_expr(&mut self, pattern_expr: &Expr, ir_expr: &IrExpr) -> bool {
        match pattern_expr {
            Expr::Any => true,

            Expr::AnyConst | Expr::Const(_) => {
                if let Some(inner) = self.get_inner_simple_expr(ir_expr) {
                    self.match_simple_expr(pattern_expr, inner)
                } else {
                    false
                }
            }

            Expr::Deref(pat_inner) => match ir_expr {
                IrExpr::Deref(ir_inner) => self.match_simple_expr(pat_inner, ir_inner),

                _ => false,
            },

            Expr::Sum(inner_patterns) | Expr::Product(inner_patterns) => {
                match inner_patterns.as_slice() {
                    [] => false,

                    [inner_pattern] => self.match_expr(inner_pattern, ir_expr),

                    _ => {
                        let expected_op = match pattern_expr {
                            Expr::Sum(_) => BinaryOpKind::Add,
                            Expr::Product(_) => BinaryOpKind::Mul,
                            _ => unreachable!(),
                        };

                        if let IrExpr::BinaryOp(BinaryOp { op, lhs, rhs }) = ir_expr {
                            if *op == expected_op {
                                for (lhs_index, lhs_pattern) in inner_patterns.iter().enumerate() {
                                    if !self.match_simple_expr(lhs_pattern, lhs) {
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

                                    if self.match_simple_expr(&rhs_pattern, rhs) {
                                        return true;
                                    }
                                }
                            }
                        }

                        false
                    }
                }
            }

            Expr::Condition(pat_cond) => self.match_condition_pattern(pat_cond, ir_expr),
        }
    }

    fn match_condition_pattern(&mut self, pat_cond: &ConditionExpr, ir_expr: &IrExpr) -> bool {
        match ir_expr {
            IrExpr::Unknown
            | IrExpr::Deref(_)
            | IrExpr::BinaryOp(_)
            | IrExpr::InsertBits { .. } => false,

            IrExpr::Simple(simple) => {
                // TODO: don't clone :(
                let pattern_expr = Expr::Condition(Box::new(pat_cond.clone()));
                self.match_simple_expr(&pattern_expr, simple)
            }

            IrExpr::CompareOp(ir_op) => self.match_compare_op(pat_cond, ir_op),

            IrExpr::X86Flag(flag_result) => self.match_x86_flag_condition(pat_cond, flag_result),

            IrExpr::ComplexX86ConditionCode(cc) => {
                self.match_complex_x86_flag_condition(pat_cond, *cc)
            }

            IrExpr::UnaryOp(UnaryOp {
                op: UnaryOpKind::Not,
                value,
            }) => self.match_condition_pattern(&pat_cond.inverse(), &IrExpr::Simple(*value)),
        }
    }

    fn match_simple_expr(&mut self, pattern_expr: &Expr, ir_expr: &SimpleExpr) -> bool {
        match (pattern_expr, ir_expr) {
            (Expr::Any, _) => true,

            (_, SimpleExpr::Variable(ir_var)) => self.match_var(pattern_expr, *ir_var),

            (Expr::AnyConst, SimpleExpr::Const(_)) => true,

            (Expr::Const(pat_x), SimpleExpr::Const(ir_x)) => *pat_x == *ir_x,

            (Expr::Deref(_) | Expr::Condition(_), SimpleExpr::Const(_)) => false,

            (Expr::Sum(terms), ir_expr @ SimpleExpr::Const(_)) => {
                if let [term] = &terms[..] {
                    self.match_simple_expr(term, ir_expr)
                } else {
                    false
                }
            }

            (Expr::Product(factors), ir_expr @ SimpleExpr::Const(_)) => {
                if let [factor] = &factors[..] {
                    self.match_simple_expr(factor, ir_expr)
                } else {
                    false
                }
            }
        }
    }

    fn try_expand_var(
        &mut self,
        ir_var: Variable,
    ) -> Option<(StatementAddr, &'view IrExpr, &'view ValueSources)> {
        let source_addr = get_single_value_source(self.value_sources, ir_var)?;

        if self.matcher.visited_addr_stack.contains(&source_addr) {
            // Loop!
            return None;
        }

        let entity = self.matcher.database.addr_to_entity[&source_addr];
        let (source_stmt, source_value_sources) = self.matcher.view.get(entity).unwrap();
        let Statement::Assign { lhs, rhs } = source_stmt
        else { panic!("source isn't an Assign statement") };
        debug_assert_eq!(*lhs, ir_var);

        Some((source_addr, rhs, source_value_sources))
    }

    fn match_var(&mut self, pattern_expr: &Expr, ir_var: Variable) -> bool {
        let Some((source_addr, source_expr, source_value_sources)) = self.try_expand_var(ir_var)
        else { return false };

        ExprMatcherAt::new(source_addr, source_value_sources, self.matcher)
            .match_expr(pattern_expr, source_expr)
    }

    fn get_inner_simple_expr<'b>(&mut self, expr: &'b IrExpr) -> Option<&'b SimpleExpr> {
        match expr {
            IrExpr::Unknown => None,

            IrExpr::Simple(inner) => Some(inner),

            IrExpr::BinaryOp(BinaryOp { op, lhs, rhs }) => {
                if lhs == rhs {
                    match op {
                        // x ^ x = 0
                        // x - x = 0
                        BinaryOpKind::Xor | BinaryOpKind::Sub => {
                            return Some(&SimpleExpr::Const(0));
                        }

                        // x & x = x
                        // x | x = x
                        BinaryOpKind::And | BinaryOpKind::Or => {
                            return Some(lhs);
                        }

                        _ => {}
                    }
                }

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
                if self.match_simple_expr(&const_pattern, rhs) {
                    return Some(lhs);
                } else if is_commutative && self.match_simple_expr(&const_pattern, lhs) {
                    return Some(rhs);
                }

                None
            }

            IrExpr::Deref(_)
            | IrExpr::UnaryOp(_)
            | IrExpr::CompareOp(_)
            | IrExpr::InsertBits { .. }
            | IrExpr::X86Flag { .. }
            | IrExpr::ComplexX86ConditionCode(_) => None,
        }
    }

    fn match_compare_op(&mut self, pat_cond: &ConditionExpr, ir_op: &CompareOp) -> bool {
        fn match_direct(
            matcher: &mut ExprMatcherAt,
            pat_cond: &ConditionExpr,
            ir_op: &CompareOp,
        ) -> bool {
            let unswapped_match = matcher.match_simple_expr(&pat_cond.lhs, &ir_op.lhs)
                && matcher.match_simple_expr(&pat_cond.rhs, &ir_op.rhs);
            if pat_cond.kind == ir_op.kind && unswapped_match {
                return true;
            }

            let swapped_match = matcher.match_simple_expr(&pat_cond.lhs, &ir_op.rhs)
                && matcher.match_simple_expr(&pat_cond.rhs, &ir_op.lhs);
            if pat_cond.kind == ir_op.kind && pat_cond.kind.is_symmetric() && swapped_match {
                return true;
            }

            // TODO: Notify caller if we're actually returning the inverse / swapped
            if pat_cond.kind == ir_op.kind.swapped_inverse() && (unswapped_match || swapped_match) {
                return true;
            }

            false
        }

        fn match_direct_or_off_by_one(
            matcher: &mut ExprMatcherAt,
            pat_cond: &ConditionExpr,
            ir_op: &CompareOp,
        ) -> bool {
            if match_direct(matcher, pat_cond, ir_op) {
                return true;
            }

            // Handle "off-by-one" comparisons (e.g. <= 0xf instead of < 0x10)
            let offset_cmp_kind_and_value = match pat_cond.kind {
                CompareOpKind::Equal | CompareOpKind::NotEqual => None,
                CompareOpKind::LessThanUnsigned => {
                    Some((CompareOpKind::LessThanOrEqualUnsigned, -1))
                }
                CompareOpKind::LessThanOrEqualUnsigned => {
                    Some((CompareOpKind::LessThanUnsigned, 1))
                }
                CompareOpKind::LessThanSigned => Some((CompareOpKind::LessThanOrEqualSigned, -1)),
                CompareOpKind::LessThanOrEqualSigned => Some((CompareOpKind::LessThanSigned, 1)),
            };

            if let Some((kind, offset)) = offset_cmp_kind_and_value {
                if let Expr::Const(rhs) = pat_cond.rhs {
                    // TODO: don't clone :(
                    if match_direct(
                        matcher,
                        &ConditionExpr {
                            kind,
                            lhs: pat_cond.lhs.clone(),
                            rhs: Expr::Const(rhs.wrapping_add_signed(offset)),
                        },
                        ir_op,
                    ) {
                        return true;
                    }
                }

                // This works for the left-hand side, too: `0xf < x` is the same as
                // `0x10 <= x`
                if let Expr::Const(lhs) = pat_cond.lhs {
                    // TODO: don't clone :(
                    if match_direct(
                        matcher,
                        &ConditionExpr {
                            kind,
                            lhs: Expr::Const(lhs.wrapping_add_signed(-offset)),
                            rhs: pat_cond.rhs.clone(),
                        },
                        ir_op,
                    ) {
                        return true;
                    }
                }
            }

            false
        }

        if match_direct_or_off_by_one(self, pat_cond, ir_op) {
            return true;
        }

        // `0 les x && x lts y` with a constant y could be compiled to `x ltu
        // y`, so if we see `x ltu y` with a constant y, match `0 les x` and `x
        // lts y`, too. Same goes for `x leu y` etc.
        if let CompareOp {
            kind: CompareOpKind::LessThanUnsigned | CompareOpKind::LessThanOrEqualUnsigned,
            lhs: ir_x,
            rhs: ir_y,
        } = ir_op
        {
            if self.match_simple_expr(&Expr::AnyConst, ir_y) {
                // Match `0 les x`
                if self.match_compare_op(
                    pat_cond,
                    &CompareOp {
                        kind: CompareOpKind::LessThanOrEqualSigned,
                        lhs: SimpleExpr::Const(0),
                        rhs: *ir_x,
                    },
                ) {
                    return true;
                }

                // Match `x lts/les y`
                let signed_op_for_y = match ir_op.kind {
                    CompareOpKind::LessThanUnsigned => CompareOpKind::LessThanSigned,
                    CompareOpKind::LessThanOrEqualUnsigned => CompareOpKind::LessThanOrEqualSigned,
                    _ => unreachable!(),
                };
                if self.match_compare_op(
                    pat_cond,
                    &CompareOp {
                        kind: signed_op_for_y,
                        lhs: *ir_x,
                        rhs: *ir_y,
                    },
                ) {
                    return true;
                }
            }
        }

        false
    }

    fn match_x86_flag_condition(
        &mut self,
        pat_cond: &ConditionExpr,
        x86_flag_info: &X86FlagResult,
    ) -> bool {
        let X86FlagResult {
            which_flag,
            mnemonic,
            lhs,
            rhs,
            math_result,
        } = *x86_flag_info;

        match which_flag {
            // S = (math result < 0)
            X86Flag::SF => {
                if mnemonic == Mnemonic::Sub {
                    if self.match_compare_op(
                        pat_cond,
                        &CompareOp {
                            kind: CompareOpKind::LessThanSigned,
                            lhs,
                            rhs,
                        },
                    ) {
                        return true;
                    }
                } else {
                    if self.match_compare_op(
                        pat_cond,
                        &CompareOp {
                            kind: CompareOpKind::LessThanSigned,
                            lhs: math_result.into(),
                            rhs: SimpleExpr::Const(0),
                        },
                    ) {
                        return true;
                    }
                }
            }

            // Z = (math result = 0)
            X86Flag::ZF => {
                if matches!(mnemonic, Mnemonic::Sub | Mnemonic::And)
                    && lhs == rhs
                    && self.match_compare_op(
                        pat_cond,
                        &CompareOp {
                            kind: CompareOpKind::Equal,
                            lhs,
                            rhs: SimpleExpr::Const(0),
                        },
                    )
                {
                    return true;
                }

                if self.match_compare_op(
                    pat_cond,
                    &CompareOp {
                        kind: CompareOpKind::Equal,
                        lhs: math_result.into(),
                        rhs: SimpleExpr::Const(0),
                    },
                ) {
                    return true;
                }
            }

            X86Flag::CF => match mnemonic {
                // C of sub = LessThanUnsigned
                Mnemonic::Sub => {
                    if self.match_compare_op(
                        pat_cond,
                        &CompareOp {
                            kind: CompareOpKind::LessThanUnsigned,
                            lhs,
                            rhs,
                        },
                    ) {
                        return true;
                    }
                }

                // There's a carry in addition iff `result ltu lhs`, and also
                // iff `result ltu rhs` (if I'm not mistaken).
                Mnemonic::Add => {
                    if self.match_compare_op(
                        pat_cond,
                        &CompareOp {
                            kind: CompareOpKind::LessThanUnsigned,
                            lhs: math_result.into(),
                            rhs: lhs,
                        },
                    ) || self.match_compare_op(
                        pat_cond,
                        &CompareOp {
                            kind: CompareOpKind::LessThanUnsigned,
                            lhs: math_result.into(),
                            rhs,
                        },
                    ) {
                        return true;
                    }
                }

                // TODO:
                // C of bt = bit is set
                _ => todo!(
                    "CF as condition for mnemonic = {:?} @ {}",
                    mnemonic,
                    self.addr
                ),
            },

            // TODO: implement OF
            X86Flag::OF => todo!("OF as condition @ {}", self.addr),
        }

        false
    }

    fn match_complex_x86_flag_condition(
        &mut self,
        pat_cond: &ConditionExpr,
        cc: ComplexX86ConditionCode,
    ) -> bool {
        let Some((source_addr, source_flag_result, source_value_sources)) = self
            .get_complex_x86_flag_result(match cc {
                ComplexX86ConditionCode::Be => &[X86Flag::CF, X86Flag::ZF],
                ComplexX86ConditionCode::L => &[X86Flag::SF, X86Flag::OF],
                ComplexX86ConditionCode::Le => &[X86Flag::SF, X86Flag::OF, X86Flag::ZF],
            })
        else {
            eprintln!("Failed to get flags for {:?} @ {}", cc, self.addr);
            return false
        };

        let X86FlagResult {
            which_flag: _,
            mnemonic,
            lhs,
            rhs,
            math_result,
        } = *source_flag_result;

        let mut source_matcher =
            ExprMatcherAt::new(source_addr, source_value_sources, self.matcher);

        match (cc, mnemonic) {
            // BE = C or Z i.e. LessThanOrEqualUnsigned when C =
            // LessThanUnsigned
            (ComplexX86ConditionCode::Be, Mnemonic::Sub) => {
                if source_matcher.match_compare_op(
                    pat_cond,
                    &CompareOp {
                        kind: CompareOpKind::LessThanOrEqualUnsigned,
                        lhs,
                        rhs,
                    },
                ) {
                    return true;
                }
            }

            // afaict, And sets CF to 0, and Be = CF|ZF, so this is the same
            // thing as the E/Z condition code.
            (ComplexX86ConditionCode::Be, Mnemonic::And) => {
                return source_matcher.match_x86_flag_condition(
                    pat_cond,
                    &X86FlagResult {
                        which_flag: X86Flag::ZF,
                        mnemonic,
                        lhs,
                        rhs,
                        math_result,
                    },
                );
            }

            (ComplexX86ConditionCode::L, Mnemonic::Sub) => {
                if source_matcher.match_compare_op(
                    pat_cond,
                    &CompareOp {
                        kind: CompareOpKind::LessThanSigned,
                        lhs,
                        rhs,
                    },
                ) {
                    return true;
                }
            }

            // And, Test and Or set OF to 0, so L = (SF ^ OF) = SF.
            (ComplexX86ConditionCode::L, Mnemonic::And | Mnemonic::Or) => {
                return source_matcher.match_x86_flag_condition(
                    pat_cond,
                    &X86FlagResult {
                        which_flag: X86Flag::SF,
                        mnemonic,
                        lhs,
                        rhs,
                        math_result,
                    },
                );
            }

            (ComplexX86ConditionCode::Le, Mnemonic::Sub) => {
                if source_matcher.match_compare_op(
                    pat_cond,
                    &CompareOp {
                        kind: CompareOpKind::LessThanOrEqualSigned,
                        lhs,
                        rhs,
                    },
                ) {
                    return true;
                }
            }

            // LE is like L but OrEqual.
            (ComplexX86ConditionCode::Le, Mnemonic::And) if lhs == rhs => {
                if source_matcher.match_compare_op(
                    pat_cond,
                    &CompareOp {
                        kind: CompareOpKind::LessThanOrEqualSigned,
                        lhs,
                        rhs: SimpleExpr::Const(0),
                    },
                ) {
                    return true;
                }
            }

            (_, _) => {
                eprintln!(
                    "unimplemented x86 cc {:?} for mnemonic = {:?} @ {}",
                    cc, mnemonic, source_matcher.addr
                );
            }
        }

        false
    }

    /// Calls `try_expand_var` for the given flags, and return one of the
    /// `X86FlagResult`s. Verifies that they're all equivalent.
    fn get_complex_x86_flag_result(
        &mut self,
        flags: &[X86Flag],
    ) -> Option<(StatementAddr, &'view X86FlagResult, &'view ValueSources)> {
        flags
            .iter()
            .map(|flag| {
                let Some((source_addr, source_expr, source_value_sources)) =
                    self.try_expand_var(ir_flag_to_register(*flag).into())
                else {
                    eprintln!("Failed to get flag info for {} @ {}", flag, self.addr);
                    return Err(());
                };

                let IrExpr::X86Flag(source_flag_result) = source_expr
                else {
                    eprintln!("Expected flag expression @ {}, got {:?}", source_addr, source_expr);
                    return Err(());
                };

                Ok((source_addr, source_flag_result, source_value_sources))
            })
            .reduce(|opt_tup1, opt_tup2| {
                let (Ok(tup1), Ok(tup2)) = (opt_tup1, opt_tup2)
                else { return Err(()) };

                let flag_res1 = tup1.1;
                let flag_res2 = tup2.1;
                assert_eq!(
                    (
                        flag_res1.mnemonic,
                        flag_res1.lhs,
                        flag_res1.rhs,
                        flag_res1.math_result
                    ),
                    (
                        flag_res2.mnemonic,
                        flag_res2.lhs,
                        flag_res2.rhs,
                        flag_res2.math_result
                    ),
                    "inconsistent complex flag infos"
                );

                Ok(tup1)
            })
            .unwrap()
            .ok()
    }
}

pub(super) struct ExprMatcher<'db, 'view, 'query> {
    database: &'db Database,
    view: &'view View<'view, (&'query Statement, &'query ValueSources)>,
    visited_addr_stack: Vec<StatementAddr>,
}

impl<'db, 'view, 'query> ExprMatcher<'db, 'view, 'query> {
    pub fn new(
        database: &'db Database,
        view: &'view View<'view, (&'query Statement, &'query ValueSources)>,
    ) -> Self {
        Self {
            database,
            view,
            visited_addr_stack: vec![],
        }
    }

    pub fn match_expr(
        &mut self,
        addr: StatementAddr,
        value_sources: &ValueSources,
        pattern_expr: &Expr,
        ir_expr: &IrExpr,
    ) -> bool {
        ExprMatcherAt::new(addr, value_sources, self).match_expr(pattern_expr, ir_expr)
    }

    pub fn match_simple_expr(
        &mut self,
        addr: StatementAddr,
        value_sources: &ValueSources,
        pattern_expr: &Expr,
        ir_expr: &SimpleExpr,
    ) -> bool {
        ExprMatcherAt::new(addr, value_sources, self).match_simple_expr(pattern_expr, ir_expr)
    }

    pub fn database(&self) -> &'db Database {
        self.database
    }
}
