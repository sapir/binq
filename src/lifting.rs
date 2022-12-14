use anyhow::Result;
use iced_x86::{
    Decoder, Instruction, InstructionInfoFactory, InstructionInfoOptions, Mnemonic, OpAccess,
    OpKind, Register as X86Register, RflagsBits,
};

use crate::{
    database::StatementAddr,
    ir::{
        Addr64, BinaryOp, BinaryOpKind, CompareOp, CompareOpKind, ComplexX86ConditionCode, Const,
        Expr, Register, SimpleExpr, Statement, UnaryOp, UnaryOpKind, Variable,
    },
};

type OperandIndex = u32;

// iced_x86 doesn't include the flags in its Register enum, so we define them
// ourselves. We use high register indices to avoid conflicts.
pub const REG_OF: Register = Register(0xffc0);
pub const REG_SF: Register = Register(0xffc1);
pub const REG_ZF: Register = Register(0xffc2);
// pub const REG_AF: Register = Register(0xffc3);
pub const REG_CF: Register = Register(0xffc4);
// pub const REG_PF: Register = Register(0xffc5);
pub const REG_DF: Register = Register(0xffc6);
pub const REG_IF: Register = Register(0xffc7);
// pub const REG_AC: Register = Register(0xffc8);
// pub const REG_UIF: Register = Register(0xffc9);
// pub const REG_C0: Register = Register(0xffca);
// pub const REG_C1: Register = Register(0xffcb);
// pub const REG_C2: Register = Register(0xffcc);
// pub const REG_C3: Register = Register(0xffcd);

pub struct X86Lifter<'a> {
    decoder: Decoder<'a>,
    instr_info_factory: InstructionInfoFactory,
    cur_insn: Instruction,
    base_addr: Addr64,
    /// Flags not (yet) implemented in the current instruction. Used for
    /// debugging.
    unimpl_flags: u32,
}

impl<'a> X86Lifter<'a> {
    pub fn new(data: &'a [u8], base_addr: Addr64) -> Self {
        Self {
            decoder: Decoder::with_ip(64, data, base_addr, 0),
            instr_info_factory: InstructionInfoFactory::new(),
            cur_insn: Instruction::default(),
            base_addr,
            unimpl_flags: 0,
        }
    }

    pub fn cur_addr(&self) -> Addr64 {
        self.decoder.ip()
    }

    pub fn set_cur_addr(&mut self, addr: Addr64) {
        let offset = addr.checked_sub(self.base_addr).unwrap();
        let offset = offset.try_into().unwrap();
        self.decoder.set_position(offset).unwrap();
        self.decoder.set_ip(addr);
    }

    fn decode_next(&mut self) {
        self.decoder.decode_out(&mut self.cur_insn);
    }

    pub fn lift_block(&mut self) -> Result<Vec<(StatementAddr, Statement)>> {
        let mut out = Output::new(self.cur_addr());

        loop {
            out.next_addr = StatementAddr {
                asm_addr: self.cur_addr(),
                ir_index: 0,
            };

            self.decode_next();
            if self.cur_insn.is_invalid() {
                break;
            }

            self.lift_cur(&mut out)?;

            if matches!(
                out.inner
                    .last()
                    .unwrap_or_else(|| panic!("no lifting output for {}", self.cur_insn))
                    .1,
                Statement::Jump { .. }
            ) {
                // End of basic block
                break;
            }
        }

        Ok(out.finish())
    }

    fn lift_cur(&mut self, out: &mut Output) -> Result<()> {
        use Mnemonic::*;

        self.unimpl_flags = self.cur_insn.rflags_modified();

        let mnemonic = self.cur_insn.mnemonic();
        match mnemonic {
            Mov => {
                let rhs = self.op_to_expr(out, 1);

                match self.rough_op_kind(0) {
                    RoughOpKind::Register => {
                        let lhs = self.op_to_reg(0).into();
                        out.push(Statement::Assign { lhs, rhs });
                    }

                    // You can't set an immediate value to anything.
                    RoughOpKind::Immediate => unreachable!(),

                    RoughOpKind::Memory => {
                        let rhs = out.expr_to_simple(rhs);
                        let addr = self.memory_access(0).to_addr_simple_expr(out);
                        out.push(Statement::Store { addr, value: rhs });
                    }
                }
            }

            Lea => {
                let rhs = self.memory_access(1).to_addr_simple_expr(out).into();
                let lhs = self.op_to_reg(0).into();
                out.push(Statement::Assign { lhs, rhs });
            }

            Add | Adc | Sub | Cmp | Sbb | Shl | Shr | Sar | Rol | Ror | And | Test | Or | Xor => {
                let rhs = self.op_to_expr(out, 1);
                let rhs = out.expr_to_simple(rhs);

                let lhs: SimpleExpr;
                let lvalue: Lvalue;
                match self.rough_op_kind(0) {
                    RoughOpKind::Register => {
                        let lhs_var = Variable::Register(self.op_to_reg(0));
                        lhs = SimpleExpr::Variable(lhs_var);
                        lvalue = Lvalue::Variable(lhs_var);
                    }

                    RoughOpKind::Immediate => unreachable!(),

                    RoughOpKind::Memory => {
                        let lhs_addr = self.memory_access(0).to_addr_simple_expr(out);
                        lhs = out.expr_to_simple(Expr::Deref(lhs_addr));
                        lvalue = Lvalue::Memory { addr: lhs_addr };
                    }
                }

                let value = match mnemonic {
                    Adc => {
                        let x = out.expr_to_simple(Expr::BinaryOp(BinaryOp {
                            op: BinaryOpKind::Add,
                            lhs,
                            rhs,
                        }));
                        out.save_expr_in_temp(Expr::BinaryOp(BinaryOp {
                            op: BinaryOpKind::Add,
                            lhs: x,
                            rhs: REG_CF.into(),
                        }))
                    }

                    Sbb => {
                        let x = out.expr_to_simple(Expr::BinaryOp(BinaryOp {
                            op: BinaryOpKind::Sub,
                            lhs,
                            rhs,
                        }));
                        out.save_expr_in_temp(Expr::BinaryOp(BinaryOp {
                            op: BinaryOpKind::Sub,
                            lhs: x,
                            rhs: REG_CF.into(),
                        }))
                    }

                    _ => {
                        let op = match mnemonic {
                            Add => BinaryOpKind::Add,
                            Sub | Cmp => BinaryOpKind::Sub,
                            Shl => BinaryOpKind::Shl,
                            Shr => BinaryOpKind::Shr,
                            Sar => BinaryOpKind::Sar,
                            Rol => BinaryOpKind::Rol,
                            Ror => BinaryOpKind::Ror,
                            And | Test => BinaryOpKind::And,
                            Or => BinaryOpKind::Or,
                            Xor => BinaryOpKind::Xor,
                            _ => unreachable!(),
                        };

                        out.save_expr_in_temp(Expr::BinaryOp(BinaryOp { op, lhs, rhs }))
                    }
                };

                for f in iter_rflags_bits(
                    self.cur_insn.rflags_modified() & !self.cur_insn.rflags_undefined(),
                ) {
                    let flag_reg = f.register();
                    self.set_flag(
                        out,
                        f,
                        flag_reg,
                        Expr::X86Flag {
                            flag_reg,
                            from_expr: value,
                        },
                    );
                }

                if !matches!(mnemonic, Cmp | Test) {
                    out.push(lvalue.make_assign_stmt(value.into()));
                }
            }

            Push => {
                let value = self.op_to_expr(out, 0);
                let value = out.expr_to_simple(value);
                out.push_to_stack(value);
            }

            Pop => {
                let lhs = self.op_to_reg(0);
                let rhs = out.pop_from_stack();
                out.push(Statement::Assign {
                    lhs: lhs.into(),
                    rhs: rhs.into(),
                });
            }

            // jp and jnp aren't implemented
            Jmp | Jo | Jno | Jb | Jae | Je | Jne | Jbe | Ja | Js | Jns | Jl | Jge | Jle | Jg => {
                let target = self.op_to_expr(out, 0);
                let target = out.expr_to_simple(target);

                let condition = match mnemonic {
                    Jmp => None,
                    Jo => Some(out.cc_expr(ConditionCode::O)),
                    Jno => Some(out.cc_expr(ConditionCode::No)),
                    Jb => Some(out.cc_expr(ConditionCode::B)),
                    Jae => Some(out.cc_expr(ConditionCode::Ae)),
                    Je => Some(out.cc_expr(ConditionCode::E)),
                    Jne => Some(out.cc_expr(ConditionCode::Ne)),
                    Jbe => Some(out.cc_expr(ConditionCode::Be)),
                    Ja => Some(out.cc_expr(ConditionCode::A)),
                    Js => Some(out.cc_expr(ConditionCode::S)),
                    Jns => Some(out.cc_expr(ConditionCode::Ns)),
                    Jl => Some(out.cc_expr(ConditionCode::L)),
                    Jge => Some(out.cc_expr(ConditionCode::Ge)),
                    Jle => Some(out.cc_expr(ConditionCode::Le)),
                    Jg => Some(out.cc_expr(ConditionCode::G)),
                    _ => unreachable!(),
                };

                let condition = condition.map(|c| out.expr_to_simple(c));

                out.push(Statement::Jump {
                    target,
                    is_return: false,
                    condition,
                });
            }

            // setp and setnp aren't implemented
            Seto | Setno | Setb | Setae | Sete | Setne | Setbe | Seta | Sets | Setns | Setl
            | Setge | Setle | Setg => {
                let reg = self.op_to_reg(0);

                let value = match mnemonic {
                    Seto => out.cc_expr(ConditionCode::O),
                    Setno => out.cc_expr(ConditionCode::No),
                    Setb => out.cc_expr(ConditionCode::B),
                    Setae => out.cc_expr(ConditionCode::Ae),
                    Sete => out.cc_expr(ConditionCode::E),
                    Setne => out.cc_expr(ConditionCode::Ne),
                    Setbe => out.cc_expr(ConditionCode::Be),
                    Seta => out.cc_expr(ConditionCode::A),
                    Sets => out.cc_expr(ConditionCode::S),
                    Setns => out.cc_expr(ConditionCode::Ns),
                    Setl => out.cc_expr(ConditionCode::L),
                    Setge => out.cc_expr(ConditionCode::Ge),
                    Setle => out.cc_expr(ConditionCode::Le),
                    Setg => out.cc_expr(ConditionCode::G),
                    _ => unreachable!(),
                };

                out.push(Statement::Assign {
                    lhs: reg.into(),
                    rhs: value,
                });
            }

            Call => {
                let return_addr = self.cur_insn.next_ip();
                out.push_to_stack(return_addr.into());

                let target = self.op_to_expr(out, 0);
                let target = out.expr_to_simple(target);
                out.push(Statement::Call { target });
            }

            Ret => {
                let target = out.pop_from_stack();

                out.push(Statement::Jump {
                    target,
                    is_return: true,
                    condition: None,
                });
            }

            Nop | Endbr32 | Endbr64 => {
                out.push(Statement::Nop);
            }

            Hlt => {
                out.push(Statement::Intrinsic);
            }

            INVALID => unreachable!(),

            _ => {
                self.set_flags_to_unknown(out, self.cur_insn.rflags_written());

                let instr_info = self
                    .instr_info_factory
                    .info_options(&self.cur_insn, InstructionInfoOptions::NO_MEMORY_USAGE);
                // TODO: handle instructions with is_save_restore_instruction
                for reg in instr_info.used_registers() {
                    match reg.access() {
                        OpAccess::Write
                        | OpAccess::CondWrite
                        | OpAccess::ReadWrite
                        | OpAccess::ReadCondWrite => {
                            // Register is written but we aren't implementing
                            // it, so set it to "unknown".
                            out.push(Statement::Assign {
                                lhs: wrap_x86_reg(reg.register()).into(),
                                rhs: Expr::Unknown,
                            });
                        }

                        OpAccess::None
                        | OpAccess::Read
                        | OpAccess::CondRead
                        | OpAccess::NoMemAccess => {
                            // Register isn't being written
                        }
                    }
                }

                out.push(Statement::Intrinsic);
                // println!("{} ({:x?})", self.cur_insn, self.cur_insn);
                // todo!("{} ({:x?})", self.cur_insn, self.cur_insn);
            }
        }

        self.fill_in_easy_rflags(out);

        for f in iter_rflags_bits(self.unimpl_flags) {
            println!("unimplemented flag: {:?} in {}", f, self.cur_insn);
        }

        Ok(())
    }

    fn op_kind(&self, index: OperandIndex) -> OpKind {
        self.cur_insn.op_kind(index)
    }

    fn rough_op_kind(&self, index: OperandIndex) -> RoughOpKind {
        RoughOpKind::of(self.op_kind(index))
    }

    /// Returns `Register(X86Register::None)` if the operand isn't a register.
    fn op_to_reg(&self, index: OperandIndex) -> Register {
        wrap_x86_reg(self.cur_insn.op_register(index))
    }

    fn op_to_imm(&self, index: OperandIndex) -> u64 {
        match self.cur_insn.op_kind(index) {
            OpKind::Register => {
                unreachable!("attempt to parse a register operand as an immediate value")
            }

            OpKind::NearBranch16 | OpKind::NearBranch32 | OpKind::NearBranch64 => {
                self.cur_insn.near_branch_target()
            }

            // TODO
            OpKind::FarBranch16 | OpKind::FarBranch32 => todo!("far branch"),

            OpKind::Immediate8 => self.cur_insn.immediate8().into(),
            OpKind::Immediate8_2nd => self.cur_insn.immediate8_2nd().into(),
            OpKind::Immediate16 => self.cur_insn.immediate16().into(),
            OpKind::Immediate32 => self.cur_insn.immediate32().into(),
            OpKind::Immediate64 => self.cur_insn.immediate64(),
            OpKind::Immediate8to16 => i64::from(self.cur_insn.immediate8to16()) as u64,
            OpKind::Immediate8to32 => i64::from(self.cur_insn.immediate8to32()) as u64,
            OpKind::Immediate8to64 => self.cur_insn.immediate8to64() as u64,
            OpKind::Immediate32to64 => self.cur_insn.immediate32to64() as u64,

            OpKind::MemorySegSI
            | OpKind::MemorySegESI
            | OpKind::MemorySegRSI
            | OpKind::MemorySegDI
            | OpKind::MemorySegEDI
            | OpKind::MemorySegRDI
            | OpKind::MemoryESDI
            | OpKind::MemoryESEDI
            | OpKind::MemoryESRDI
            | OpKind::Memory => {
                unreachable!("attempt to parse a memory operand as an immediate value")
            }
        }
    }

    fn op_to_expr(&self, out: &mut Output, index: OperandIndex) -> Expr {
        match self.rough_op_kind(index) {
            RoughOpKind::Register => SimpleExpr::Variable(self.op_to_reg(index).into()).into(),
            RoughOpKind::Immediate => SimpleExpr::Const(self.op_to_imm(index)).into(),
            RoughOpKind::Memory => {
                let addr = self.memory_access(index).to_addr_simple_expr(out);
                Expr::Deref(addr)
            }
        }
    }

    fn memory_access(&self, index: OperandIndex) -> MemoryAccess {
        match self.op_kind(index) {
            OpKind::Register
            | OpKind::NearBranch16
            | OpKind::NearBranch32
            | OpKind::NearBranch64
            | OpKind::FarBranch16
            | OpKind::FarBranch32
            | OpKind::Immediate8
            | OpKind::Immediate8_2nd
            | OpKind::Immediate16
            | OpKind::Immediate32
            | OpKind::Immediate64
            | OpKind::Immediate8to16
            | OpKind::Immediate8to32
            | OpKind::Immediate8to64
            | OpKind::Immediate32to64 => {
                unreachable!("attempt to parse a non-memory operand as a memory operand")
            }

            OpKind::MemorySegSI
            | OpKind::MemorySegESI
            | OpKind::MemorySegRSI
            | OpKind::MemorySegDI
            | OpKind::MemorySegEDI
            | OpKind::MemorySegRDI
            | OpKind::MemoryESDI
            | OpKind::MemoryESEDI
            | OpKind::MemoryESRDI => todo!("segmented memory access"),

            OpKind::Memory => MemoryAccess {
                base: self.cur_insn.memory_base(),
                index: self.cur_insn.memory_index(),
                index_scale: self.cur_insn.memory_index_scale(),
                displacement: self.cur_insn.memory_displacement64(),
                size: self.cur_insn.memory_size().size().try_into().unwrap(),
            },
        }
    }

    fn set_flag(&mut self, out: &mut Output, flag: Rflag, reg: Register, value: Expr) {
        let flag_bits = flag as u32;
        if (self.unimpl_flags & flag_bits) == 0 {
            println!(
                "double or unnecessary implementation for {:?} in {} ({:?})",
                flag, self.cur_insn, self.cur_insn
            );
        }
        self.unimpl_flags &= !flag_bits;

        out.push(Statement::Assign {
            lhs: reg.into(),
            rhs: value,
        });
    }

    fn set_sf(&mut self, out: &mut Output, for_value: SimpleExpr) {
        self.set_flag(
            out,
            Rflag::SF,
            REG_SF,
            Expr::CompareOp(CompareOp {
                kind: CompareOpKind::LessThanSigned,
                lhs: for_value,
                rhs: 0.into(),
            }),
        );
    }

    fn set_zf(&mut self, out: &mut Output, for_value: SimpleExpr) {
        self.set_flag(
            out,
            Rflag::ZF,
            REG_ZF,
            Expr::CompareOp(CompareOp {
                kind: CompareOpKind::NotEqual,
                lhs: for_value,
                rhs: 0.into(),
            }),
        );
    }

    fn set_flags_to_unknown(&mut self, out: &mut Output, bits: u32) {
        for f in iter_rflags_bits(bits) {
            self.set_flag(out, f, f.register(), Expr::Unknown);
        }
    }

    fn fill_in_easy_rflags(&mut self, out: &mut Output) {
        self.set_flags_to_unknown(out, self.cur_insn.rflags_undefined());

        for f in iter_rflags_bits(self.cur_insn.rflags_cleared() & self.unimpl_flags) {
            self.set_flag(out, f, f.register(), 0.into());
        }

        for f in iter_rflags_bits(self.cur_insn.rflags_set() & self.unimpl_flags) {
            self.set_flag(out, f, f.register(), 1.into());
        }
    }
}

#[derive(Clone, Copy)]
enum RoughOpKind {
    Register,
    Immediate,
    Memory,
}

impl RoughOpKind {
    fn of(op_kind: OpKind) -> Self {
        match op_kind {
            OpKind::Register => Self::Register,

            OpKind::NearBranch16
            | OpKind::NearBranch32
            | OpKind::NearBranch64
            | OpKind::FarBranch16
            | OpKind::FarBranch32
            | OpKind::Immediate8
            | OpKind::Immediate8_2nd
            | OpKind::Immediate16
            | OpKind::Immediate32
            | OpKind::Immediate64
            | OpKind::Immediate8to16
            | OpKind::Immediate8to32
            | OpKind::Immediate8to64
            | OpKind::Immediate32to64 => Self::Immediate,

            OpKind::MemorySegSI
            | OpKind::MemorySegESI
            | OpKind::MemorySegRSI
            | OpKind::MemorySegDI
            | OpKind::MemorySegEDI
            | OpKind::MemorySegRDI
            | OpKind::MemoryESDI
            | OpKind::MemoryESEDI
            | OpKind::MemoryESRDI
            | OpKind::Memory => Self::Memory,
        }
    }
}

struct Output {
    inner: Vec<(StatementAddr, Statement)>,
    next_addr: StatementAddr,
}

impl Output {
    fn new(addr: Addr64) -> Self {
        Self {
            inner: vec![],
            next_addr: StatementAddr {
                asm_addr: addr,
                ir_index: 0,
            },
        }
    }

    fn finish(self) -> Vec<(StatementAddr, Statement)> {
        self.inner
    }

    fn push(&mut self, stmt: Statement) {
        self.inner.push((self.next_addr, stmt));
        self.next_addr.ir_index += 1;
    }

    fn save_expr_in_temp(&mut self, expr: Expr) -> Variable {
        let temp = Variable::new_temp();
        self.push(Statement::Assign {
            lhs: temp,
            rhs: expr,
        });
        temp
    }

    /// Convert an `Expr` to a `SimpleExpr` by assigning it to a `Temp`, if
    /// necessary.
    fn expr_to_simple(&mut self, expr: Expr) -> SimpleExpr {
        if let Expr::Simple(simple) = expr {
            simple
        } else {
            self.save_expr_in_temp(expr).into()
        }
    }

    fn push_to_stack(&mut self, value: SimpleExpr) {
        // TODO: depends on 32-/64- bit mode
        let stack_register = X86Register::RSP;
        let size = stack_register.size();
        let stack_register = wrap_x86_reg(stack_register);
        self.push(Statement::Assign {
            lhs: stack_register.into(),
            rhs: Expr::BinaryOp(BinaryOp {
                op: BinaryOpKind::Sub,
                lhs: stack_register.into(),
                rhs: SimpleExpr::Const(size.try_into().unwrap()),
            }),
        });
        self.push(Statement::Store {
            addr: stack_register.into(),
            value,
        });
    }

    fn pop_from_stack(&mut self) -> SimpleExpr {
        // TODO: depends on 32-/64- bit mode
        let stack_register = X86Register::RSP;
        let size = stack_register.size();
        let stack_register = wrap_x86_reg(stack_register);
        let value = self.expr_to_simple(Expr::Deref(stack_register.into()));

        self.push(Statement::Assign {
            lhs: stack_register.into(),
            rhs: Expr::BinaryOp(BinaryOp {
                op: BinaryOpKind::Add,
                lhs: stack_register.into(),
                rhs: SimpleExpr::Const(size.try_into().unwrap()),
            }),
        });

        value
    }

    fn not_simple_expr(&mut self, simple_expr: SimpleExpr) -> Expr {
        Expr::UnaryOp(UnaryOp {
            op: UnaryOpKind::Not,
            value: simple_expr,
        })
    }

    fn cc_expr(&mut self, cc: ConditionCode) -> Expr {
        match cc {
            ConditionCode::O => REG_OF.into(),
            ConditionCode::No => self.not_simple_expr(REG_OF.into()),

            ConditionCode::B => REG_CF.into(),
            ConditionCode::Ae => self.not_simple_expr(REG_CF.into()),

            ConditionCode::E => REG_ZF.into(),
            ConditionCode::Ne => self.not_simple_expr(REG_ZF.into()),

            ConditionCode::Be => Expr::ComplexX86ConditionCode(ComplexX86ConditionCode::Be),
            ConditionCode::A => {
                let cc_val =
                    self.expr_to_simple(Expr::ComplexX86ConditionCode(ComplexX86ConditionCode::Be));
                self.not_simple_expr(cc_val)
            }

            ConditionCode::S => REG_SF.into(),
            ConditionCode::Ns => self.not_simple_expr(REG_SF.into()),

            ConditionCode::P => todo!("x86 'p' condition code not implemented"),
            ConditionCode::Np => todo!("x86 'np' condition code not implemented"),

            ConditionCode::L => Expr::ComplexX86ConditionCode(ComplexX86ConditionCode::L),
            ConditionCode::Ge => {
                let cc_val =
                    self.expr_to_simple(Expr::ComplexX86ConditionCode(ComplexX86ConditionCode::L));
                self.not_simple_expr(cc_val)
            }

            ConditionCode::Le => Expr::ComplexX86ConditionCode(ComplexX86ConditionCode::Le),
            ConditionCode::G => {
                let cc_val =
                    self.expr_to_simple(Expr::ComplexX86ConditionCode(ComplexX86ConditionCode::Le));
                self.not_simple_expr(cc_val)
            }
        }
    }
}

#[derive(Clone)]
struct MemoryAccess {
    base: X86Register,
    index: X86Register,
    index_scale: u32,
    displacement: Const,
    size: u8,
}

impl MemoryAccess {
    fn to_addr_simple_expr(&self, out: &mut Output) -> SimpleExpr {
        let mut addends: Vec<SimpleExpr> = Vec::with_capacity(3);

        if self.base != X86Register::None {
            addends.push(wrap_x86_reg(self.base).into());
        }

        if self.index != X86Register::None {
            let mut index = SimpleExpr::from(wrap_x86_reg(self.index));

            if self.index_scale != 1 {
                index = out.expr_to_simple(Expr::BinaryOp(BinaryOp {
                    op: BinaryOpKind::Mul,
                    lhs: index,
                    rhs: SimpleExpr::Const(self.index_scale.into()),
                }));
            }

            addends.push(index);
        }

        // Ignore 0 displacement if we have a register or index.
        if self.displacement != 0 || addends.is_empty() {
            addends.push(self.displacement.into());
        }

        addends
            .into_iter()
            .reduce(|a, b| {
                out.expr_to_simple(Expr::BinaryOp(BinaryOp {
                    op: BinaryOpKind::Add,
                    lhs: a,
                    rhs: b,
                }))
            })
            .unwrap()
    }
}

fn wrap_x86_reg(reg: X86Register) -> Register {
    Register((reg as usize).try_into().unwrap())
}

/// Only flags that we actually use are included
#[derive(Clone, Copy, Debug)]
#[repr(u32)]
enum Rflag {
    /// `RFLAGS.OF`
    OF = RflagsBits::OF,
    /// `RFLAGS.SF`
    SF = RflagsBits::SF,
    /// `RFLAGS.ZF`
    ZF = RflagsBits::ZF,
    // /// `RFLAGS.AF`
    // AF = RflagsBits::AF,
    /// `RFLAGS.CF`
    CF = RflagsBits::CF,
    // /// `RFLAGS.PF`
    // PF = RflagsBits::PF,
    /// `RFLAGS.DF`
    DF = RflagsBits::DF,
    /// `RFLAGS.IF`
    IF = RflagsBits::IF,
    // /// `RFLAGS.AC`
    // AC = RflagsBits::AC,
    // /// `UIF`
    // UIF = RflagsBits::UIF,
    // /// FPU status word bit `C0`
    // C0 = RflagsBits::C0,
    // /// FPU status word bit `C1`
    // C1 = RflagsBits::C1,
    // /// FPU status word bit `C2`
    // C2 = RflagsBits::C2,
    // /// FPU status word bit `C3`
    // C3 = RflagsBits::C3,
}

impl Rflag {
    fn register(self) -> Register {
        match self {
            Rflag::OF => REG_OF,
            Rflag::SF => REG_SF,
            Rflag::ZF => REG_ZF,
            Rflag::CF => REG_CF,
            Rflag::DF => REG_DF,
            Rflag::IF => REG_IF,
        }
    }
}

/// Iterate over [`RflagsBits`] values. Only flags that we actually use are
/// included
fn iter_rflags_bits(value: u32) -> impl Iterator<Item = Rflag> {
    use Rflag::*;

    [OF, SF, ZF, CF, DF, IF]
        .into_iter()
        .filter(move |f| (value & (*f as u32)) != 0)
}

#[derive(Clone, Copy)]
enum Lvalue {
    Variable(Variable),
    Memory { addr: SimpleExpr },
}

impl Lvalue {
    fn make_assign_stmt(self, value: SimpleExpr) -> Statement {
        match self {
            Lvalue::Variable(var) => Statement::Assign {
                lhs: var,
                rhs: value.into(),
            },

            Lvalue::Memory { addr } => Statement::Store { addr, value },
        }
    }
}

#[derive(Clone, Copy)]
enum ConditionCode {
    O,
    No,
    B,
    Ae,
    E,
    Ne,
    Be,
    A,
    S,
    Ns,
    P,
    Np,
    L,
    Ge,
    Le,
    G,
}
