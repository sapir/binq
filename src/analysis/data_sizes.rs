use std::collections::HashMap;

use crate::{
    database::{Database, StatementAddr},
    ir::{BinaryOp, ChangeWidthOp, Expr, SimpleExpr, SizeBits, Statement, Temp, UnaryOp, Variable},
    lifting::{get_x86_register_size, unwrap_x86_reg},
};

pub fn analyze_data_sizes(db: &Database) {
    let mut tracker = DataSizeTracker::default();

    let mut query = db.world.query::<(&StatementAddr, &Statement)>();
    let mut assigns: Vec<(StatementAddr, &Statement)> = query
        .into_iter()
        .filter_map(|(_entity, (addr, stmt))| {
            if matches!(stmt, Statement::Assign { .. }) {
                Some((*addr, stmt))
            } else {
                None
            }
        })
        .collect();
    assigns.sort_unstable_by_key(|(addr, _)| *addr);

    for (addr, stmt) in assigns {
        let Statement::Assign { lhs, rhs } = stmt else { continue };

        let lhs_size = tracker.size_of_var(*lhs);
        let rhs_size = tracker.size_of_expr(rhs);

        if let (Some(lhs_size), Some(rhs_size)) = (lhs_size, rhs_size) {
            assert_eq!(
                lhs_size, rhs_size,
                "lhs size != rhs_size in {stmt} @ {addr}"
            );
        }

        if let Variable::Temp(t) = *lhs {
            if !matches!(rhs, Expr::Unknown) {
                tracker.set_temp_size(
                    t,
                    rhs_size.unwrap_or_else(|| panic!("unknown temp expr size in {stmt} @ {addr}")),
                );
            }
        }
    }
}

#[derive(Default)]
struct DataSizeTracker {
    temp_sizes: HashMap<Temp, SizeBits>,
}

impl DataSizeTracker {
    fn set_temp_size(&mut self, t: Temp, size: SizeBits) {
        self.temp_sizes.insert(t, size);
    }

    fn size_of_var(&self, var: Variable) -> Option<SizeBits> {
        match var {
            Variable::Register(reg) => {
                if let Some(x86_reg) = unwrap_x86_reg(reg) {
                    Some(get_x86_register_size(x86_reg).into())
                } else {
                    // It's a fake register, so it currently must be a flag.
                    Some(SizeBits(1))
                }
            }

            Variable::Temp(t) => self.temp_sizes.get(&t).copied(),
        }
    }

    fn size_of_simple_expr(&self, expr: SimpleExpr) -> Option<SizeBits> {
        match expr {
            SimpleExpr::Const(_) => None,
            SimpleExpr::Variable(var) => self.size_of_var(var),
        }
    }

    fn size_of_expr(&self, expr: &Expr) -> Option<SizeBits> {
        match expr {
            Expr::Unknown => None,

            Expr::Simple(value) | Expr::UnaryOp(UnaryOp { op: _, value }) => {
                self.size_of_simple_expr(*value)
            }

            Expr::ChangeWidth(ChangeWidthOp { new_size, .. }) => Some(*new_size),

            Expr::Deref { ptr: _, size } => Some((*size).into()),

            Expr::BinaryOp(BinaryOp { op, lhs, rhs }) => {
                let lhs_size = self.size_of_simple_expr(*lhs);
                let rhs_size = if op.is_shift_or_rotate() {
                    // Ignore the size of the rhs for shifts and rotates.
                    None
                } else {
                    self.size_of_simple_expr(*rhs)
                };
                if let (Some(lhs_size), Some(rhs_size)) = (lhs_size, rhs_size) {
                    assert_eq!(lhs_size, rhs_size, "size mismatch in {expr}");
                }
                lhs_size.or(rhs_size)
            }

            Expr::InsertBits {
                lhs,
                shift,
                num_bits,
                rhs,
            } => {
                let lhs_size = self.size_of_simple_expr(*lhs);
                let rhs_size = self.size_of_simple_expr(*rhs);

                if let Some(lhs_size) = lhs_size {
                    assert!(
                        lhs_size.bits() >= (*shift) + (*num_bits),
                        "size mismatch in {expr} (lhs size is {lhs_size:?})"
                    );
                }
                if let Some(rhs_size) = rhs_size {
                    assert!(
                        rhs_size.bits() <= (*num_bits),
                        "size mismatch in {expr} (rhs size is {rhs_size:?})"
                    );
                }

                lhs_size
            }

            // TODO: this is currently true but it's not supposed to be
            // required. ExtractBits could extract 5 bits from a u8 and place
            // them in a u32. The size should then be 32, not 5 or 8.
            Expr::ExtractBits { num_bits, .. } => Some(SizeBits(*num_bits)),

            Expr::CompareOp(_) | Expr::X86Flag(_) | Expr::ComplexX86ConditionCode(_) => {
                Some(SizeBits(1))
            }
        }
    }
}
