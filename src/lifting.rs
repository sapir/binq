use std::cmp::Ordering;

use anyhow::Result;
use iced_x86::{
    Decoder, Instruction, InstructionInfoFactory, InstructionInfoOptions, Mnemonic, OpAccess,
    OpKind, Register as X86Register, RflagsBits,
};

use crate::{
    database::{NextStatementAddr, StatementAddr},
    ir::{
        Addr64, BinaryOp, BinaryOpKind, ChangeWidthKind, ChangeWidthOp, CompareOp, CompareOpKind,
        ComplexX86ConditionCode, Const, Expr, Register, SimpleExpr, SizeBytes, Statement, Temp,
        UnaryOp, UnaryOpKind, Variable, X86Flag, X86FlagResult,
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
// pub const REG_DF: Register = Register(0xffc6);
// pub const REG_IF: Register = Register(0xffc7);
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
    pub fn new(bitness: u32, data: &'a [u8], base_addr: Addr64) -> Self {
        Self {
            decoder: Decoder::with_ip(bitness, data, base_addr, 0),
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

    pub fn lift_block(
        &mut self,
        mut should_stop_before: impl FnMut(Addr64) -> bool,
    ) -> Result<Vec<(StatementAddr, NextStatementAddr, Statement)>> {
        let mut out = Output::new(self.decoder.bitness(), self.cur_addr());

        loop {
            if should_stop_before(self.cur_addr()) {
                break;
            }

            out.next_addr = StatementAddr {
                asm_addr: self.cur_addr(),
                ir_index: 0,
            };
            out.set_next_of_last(self.cur_addr());

            self.decode_next();
            if self.cur_insn.is_invalid() {
                // Set the invalid instruction's address as the next address.
                // Don't just `break` because then we use `cur_addr()` for the
                // next address, and `cur_addr` maybe already points past the
                // invalid instruction.
                let next_addr = out.next_addr.asm_addr;
                return Ok(out.finish(next_addr));
            }

            self.lift_cur(&mut out)?;

            if matches!(
                out.inner
                    .last()
                    .unwrap_or_else(|| panic!("no lifting output for {}", self.cur_insn))
                    .2,
                Statement::Jump { .. }
            ) {
                // End of basic block
                break;
            }
        }

        Ok(out.finish(self.cur_addr()))
    }

    fn lift_cur(&mut self, out: &mut Output) -> Result<()> {
        use Mnemonic::*;

        let temp_before = Temp::peek_next_id();
        let mut is_jmp = false;

        self.unimpl_flags = self.cur_insn.rflags_modified();

        let mnemonic = self.cur_insn.mnemonic();
        match mnemonic {
            Mov | Movsx | Movzx => {
                let mut rhs = self.op_to_expr(out, 1);

                let extend_kind = match mnemonic {
                    Mov => None,
                    Movsx => Some(ChangeWidthKind::SignExtend),
                    Movzx => Some(ChangeWidthKind::ZeroExtend),
                    _ => unreachable!(),
                };
                if let Some(extend_kind) = extend_kind {
                    rhs = Expr::ChangeWidth(ChangeWidthOp {
                        kind: extend_kind,
                        new_size: self.op_size(0).into(),
                        inner: out.expr_to_simple(rhs),
                    });
                }

                match self.rough_op_kind(0) {
                    RoughOpKind::Register => {
                        let lhs = self.op_to_reg(0).into();
                        out.push_assign(lhs, rhs);
                    }

                    // You can't set an immediate value to anything.
                    RoughOpKind::Immediate => unreachable!(),

                    RoughOpKind::Memory => {
                        let rhs = out.expr_to_simple(rhs);
                        let mem_access = self.memory_access(0);
                        let addr = mem_access.to_addr_simple_expr(out);
                        out.push(Statement::Store {
                            addr,
                            value: rhs,
                            size: SizeBytes(mem_access.size),
                        });
                    }
                }
            }

            Lea => {
                let addr_simple_expr = self.memory_access(1).to_addr_simple_expr(out);

                let lhs = self.op_to_reg(0).into();

                let addr_size_bits = self.decoder.bitness();
                let lhs_size = self.op_size(0);
                let change_width_kind = match lhs_size.bits().cmp(&addr_size_bits) {
                    Ordering::Less => Some(ChangeWidthKind::Truncate),
                    Ordering::Equal => None,
                    Ordering::Greater => Some(ChangeWidthKind::ZeroExtend),
                };
                let rhs = if let Some(kind) = change_width_kind {
                    Expr::ChangeWidth(ChangeWidthOp {
                        kind,
                        new_size: lhs_size.into(),
                        inner: addr_simple_expr,
                    })
                } else {
                    Expr::from(addr_simple_expr)
                };

                out.push(Statement::Assign { lhs, rhs });
            }

            Add | Adc | Sub | Cmp | Sbb | Inc | Dec | Shl | Shr | Sar | Rol | Ror | And | Test
            | Or | Xor => {
                let rhs = match mnemonic {
                    Inc | Dec => {
                        debug_assert_eq!(self.cur_insn.op_count(), 1);
                        SimpleExpr::Const(1)
                    }

                    _ => {
                        debug_assert_eq!(self.cur_insn.op_count(), 2);
                        let rhs = self.op_to_expr(out, 1);
                        out.expr_to_simple(rhs)
                    }
                };

                let lvalue = self.op_to_lvalue(out, 0);

                let lhs = self.op_to_expr(out, 0);
                let lhs = out.expr_to_simple(lhs);

                let value = match mnemonic {
                    Adc => {
                        let x = out.expr_to_simple(Expr::BinaryOp(BinaryOp {
                            op: BinaryOpKind::Add,
                            lhs,
                            rhs,
                        }));
                        let cf = out.expr_to_simple(Expr::ChangeWidth(ChangeWidthOp {
                            kind: ChangeWidthKind::ZeroExtend,
                            new_size: self.op_size(0).into(),
                            inner: REG_CF.into(),
                        }));
                        out.save_expr_in_temp(Expr::BinaryOp(BinaryOp {
                            op: BinaryOpKind::Add,
                            lhs: x,
                            rhs: cf,
                        }))
                    }

                    Sbb => {
                        let x = out.expr_to_simple(Expr::BinaryOp(BinaryOp {
                            op: BinaryOpKind::Sub,
                            lhs,
                            rhs,
                        }));
                        let cf = out.expr_to_simple(Expr::ChangeWidth(ChangeWidthOp {
                            kind: ChangeWidthKind::ZeroExtend,
                            new_size: self.op_size(0).into(),
                            inner: REG_CF.into(),
                        }));
                        out.save_expr_in_temp(Expr::BinaryOp(BinaryOp {
                            op: BinaryOpKind::Sub,
                            lhs: x,
                            rhs: cf,
                        }))
                    }

                    _ => {
                        let op = match mnemonic {
                            Add | Inc => BinaryOpKind::Add,
                            Sub | Cmp | Dec => BinaryOpKind::Sub,
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

                let mnemonic_for_flag = match mnemonic {
                    Cmp => Sub,
                    Test => And,
                    _ => mnemonic,
                };

                for f in iter_rflags_bits(
                    self.cur_insn.rflags_modified() & !self.cur_insn.rflags_undefined(),
                ) {
                    let flag_reg = f.register();
                    self.set_flag(
                        out,
                        f,
                        flag_reg,
                        Expr::X86Flag(X86FlagResult {
                            which_flag: f.ir_flag(),
                            mnemonic: mnemonic_for_flag,
                            lhs,
                            rhs,
                            math_result: value,
                        }),
                    );
                }

                if !matches!(mnemonic, Cmp | Test) {
                    out.push_assign_lvalue(lvalue, value.into());
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
                out.push_assign(lhs.into(), rhs.into());
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
                is_jmp = true;
            }

            // setp and setnp aren't implemented
            Seto | Setno | Setb | Setae | Sete | Setne | Setbe | Seta | Sets | Setns | Setl
            | Setge | Setle | Setg => {
                let lhs = self.op_to_lvalue(out, 0);

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
                let value = out.expr_to_simple(value);
                let value = Expr::ChangeWidth(ChangeWidthOp {
                    kind: ChangeWidthKind::ZeroExtend,
                    new_size: self.op_size(0).into(),
                    inner: value,
                });

                out.push_assign_lvalue(lhs, value);
            }

            Call => {
                let return_addr = self.cur_insn.next_ip();
                out.push_to_stack(return_addr.into());

                let target = self.op_to_expr(out, 0);
                let target = out.expr_to_simple(target);
                out.push(Statement::Call { target });
                is_jmp = true;
            }

            Ret => {
                let target = out.pop_from_stack();

                out.push(Statement::Jump {
                    target,
                    is_return: true,
                    condition: None,
                });
                is_jmp = true;
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
                            out.push_assign(wrap_x86_reg(reg.register()).into(), Expr::Unknown);
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

        let temp_after = Temp::peek_next_id();
        if temp_before != temp_after && !is_jmp {
            out.push(Statement::ClearTemps);
        }

        Ok(())
    }

    fn op_kind(&self, index: OperandIndex) -> OpKind {
        self.cur_insn.op_kind(index)
    }

    fn rough_op_kind(&self, index: OperandIndex) -> RoughOpKind {
        RoughOpKind::of(self.op_kind(index))
    }

    /// Retrieves a register operand. Panics if the operand isn't a register.
    fn op_to_reg(&self, index: OperandIndex) -> Register {
        let reg = self.cur_insn.op_register(index);
        assert_ne!(
            reg,
            X86Register::None,
            "tried to lift operand {index} as a register in {}",
            self.cur_insn,
        );
        wrap_x86_reg(reg)
    }

    fn op_to_imm(&self, index: OperandIndex) -> u64 {
        match self.cur_insn.op_kind(index) {
            OpKind::Register => {
                unreachable!("attempt to parse a register operand as an immediate value")
            }

            OpKind::NearBranch16 | OpKind::NearBranch32 | OpKind::NearBranch64 => {
                self.cur_insn.near_branch_target()
            }

            // TODO: include the selector somehow
            OpKind::FarBranch16 => self.cur_insn.far_branch16().into(),
            OpKind::FarBranch32 => self.cur_insn.far_branch32().into(),

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
            RoughOpKind::Register => self.op_to_reg(index).into(),
            RoughOpKind::Immediate => SimpleExpr::Const(self.op_to_imm(index)).into(),
            RoughOpKind::Memory => {
                let mem_access = self.memory_access(index);
                Expr::Deref {
                    ptr: mem_access.to_addr_simple_expr(out),
                    size: SizeBytes(mem_access.size),
                }
            }
        }
    }

    /// Returns the operand as an `Lvalue`
    fn op_to_lvalue(&self, out: &mut Output, index: OperandIndex) -> Lvalue {
        match self.rough_op_kind(index) {
            RoughOpKind::Register => Lvalue::Variable(Variable::Register(self.op_to_reg(index))),

            RoughOpKind::Immediate => unreachable!(),

            RoughOpKind::Memory => {
                let mem_access = self.memory_access(index);
                Lvalue::Memory {
                    addr: mem_access.to_addr_simple_expr(out),
                    size: SizeBytes(mem_access.size),
                }
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

    fn op_size(&self, index: u32) -> SizeBytes {
        match self.cur_insn.op_kind(index) {
            OpKind::Register => get_x86_register_size(self.cur_insn.op_register(index)),

            OpKind::Immediate8 | OpKind::Immediate8_2nd => SizeBytes(1),

            OpKind::NearBranch16
            | OpKind::FarBranch16
            | OpKind::Immediate16
            | OpKind::Immediate8to16 => SizeBytes(2),

            OpKind::NearBranch32
            | OpKind::FarBranch32
            | OpKind::Immediate32
            | OpKind::Immediate8to32 => SizeBytes(4),

            OpKind::NearBranch64
            | OpKind::Immediate64
            | OpKind::Immediate8to64
            | OpKind::Immediate32to64 => SizeBytes(8),

            OpKind::MemorySegSI
            | OpKind::MemorySegESI
            | OpKind::MemorySegRSI
            | OpKind::MemorySegDI
            | OpKind::MemorySegEDI
            | OpKind::MemorySegRDI
            | OpKind::MemoryESDI
            | OpKind::MemoryESEDI
            | OpKind::MemoryESRDI
            | OpKind::Memory => SizeBytes(self.cur_insn.memory_size().size().try_into().unwrap()),
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
    bitness: u32,
    inner: Vec<(StatementAddr, NextStatementAddr, Statement)>,
    next_addr: StatementAddr,
}

impl Output {
    fn new(bitness: u32, addr: Addr64) -> Self {
        Self {
            bitness,
            inner: vec![],
            next_addr: StatementAddr {
                asm_addr: addr,
                ir_index: 0,
            },
        }
    }

    /// Sets the `NextStatementAddr` of the last output instruction. Doesn't do
    /// anything if the output is empty.
    fn set_next_of_last(&mut self, next_asm_addr: Addr64) {
        if let Some((_, next_addr, _)) = self.inner.last_mut() {
            *next_addr = NextStatementAddr(StatementAddr {
                asm_addr: next_asm_addr,
                ir_index: 0,
            });
        }
    }

    fn finish(
        mut self,
        next_asm_addr: Addr64,
    ) -> Vec<(StatementAddr, NextStatementAddr, Statement)> {
        self.set_next_of_last(next_asm_addr);
        self.inner
    }

    fn push(&mut self, stmt: Statement) {
        let mut next_addr = self.next_addr;
        next_addr.ir_index += 1;
        self.inner
            .push((self.next_addr, NextStatementAddr(next_addr), stmt));
        self.next_addr = next_addr;
    }

    /// Add an `Assign` statement. Updates linked partial/full registers, too.
    fn push_assign(&mut self, lhs: Variable, rhs: Expr) {
        self.push(Statement::Assign { lhs, rhs });

        if let Variable::Register(reg) = lhs {
            if let Some(reg) = unwrap_x86_reg(reg) {
                for &partial_reg in get_partial_regs(reg) {
                    self.push(Statement::Assign {
                        lhs: wrap_x86_reg(partial_reg).into(),
                        rhs: Expr::ExtractBits {
                            inner: lhs.into(),
                            shift: get_partial_reg_shift(partial_reg),
                            num_bits: get_x86_register_size(partial_reg).bits(),
                        },
                    });
                }

                for &full_reg in get_full_regs(reg) {
                    if self.bitness < 32 && full_reg.is_gpr32() {
                        continue;
                    }
                    if self.bitness < 64 && full_reg.is_gpr64() {
                        continue;
                    }

                    self.push(Statement::Assign {
                        lhs: wrap_x86_reg(full_reg).into(),
                        rhs: Expr::InsertBits {
                            lhs: wrap_x86_reg(full_reg).into(),
                            shift: get_partial_reg_shift(reg),
                            num_bits: get_x86_register_size(reg).bits(),
                            rhs: lhs.into(),
                        },
                    });
                }
            }
        }
    }

    fn push_assign_lvalue(&mut self, lhs: Lvalue, rhs: Expr) {
        match lhs {
            Lvalue::Variable(var) => {
                self.push_assign(var, rhs);
            }

            Lvalue::Memory { addr, size } => {
                let rhs = self.expr_to_simple(rhs);
                self.push(Statement::Store {
                    addr,
                    value: rhs,
                    size,
                });
            }
        }
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

    fn stack_register(&self) -> X86Register {
        match self.bitness {
            16 => X86Register::SP,
            32 => X86Register::ESP,
            64 => X86Register::RSP,
            _ => unreachable!(),
        }
    }

    fn push_to_stack(&mut self, value: SimpleExpr) {
        let stack_register = self.stack_register();
        let size_bytes = get_x86_register_size(stack_register);
        let stack_register = wrap_x86_reg(stack_register);
        self.push(Statement::Assign {
            lhs: stack_register.into(),
            rhs: Expr::BinaryOp(BinaryOp {
                op: BinaryOpKind::Sub,
                lhs: stack_register.into(),
                rhs: SimpleExpr::Const(size_bytes.0.try_into().unwrap()),
            }),
        });
        self.push(Statement::Store {
            addr: stack_register.into(),
            value,
            size: size_bytes,
        });
    }

    fn pop_from_stack(&mut self) -> SimpleExpr {
        let stack_register = self.stack_register();
        let size_bytes = get_x86_register_size(stack_register);
        let stack_register = wrap_x86_reg(stack_register);
        let value = self.expr_to_simple(Expr::Deref {
            ptr: stack_register.into(),
            size: size_bytes,
        });

        self.push(Statement::Assign {
            lhs: stack_register.into(),
            rhs: Expr::BinaryOp(BinaryOp {
                op: BinaryOpKind::Add,
                lhs: stack_register.into(),
                rhs: SimpleExpr::Const(size_bytes.0.try_into().unwrap()),
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
    size: u32,
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

pub fn wrap_x86_reg(reg: X86Register) -> Register {
    Register((reg as usize).try_into().unwrap())
}

fn unwrap_x86_reg(reg: Register) -> Option<X86Register> {
    X86Register::try_from(usize::from(reg.0)).ok()
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
    // /// `RFLAGS.DF`
    // DF = RflagsBits::DF,
    // /// `RFLAGS.IF`
    // IF = RflagsBits::IF,
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
        }
    }

    fn ir_flag(self) -> X86Flag {
        match self {
            Rflag::OF => X86Flag::OF,
            Rflag::SF => X86Flag::SF,
            Rflag::ZF => X86Flag::ZF,
            Rflag::CF => X86Flag::CF,
        }
    }

    fn from_ir_flag(flag: X86Flag) -> Self {
        match flag {
            X86Flag::OF => Rflag::OF,
            X86Flag::SF => Rflag::SF,
            X86Flag::ZF => Rflag::ZF,
            X86Flag::CF => Rflag::CF,
        }
    }
}

pub fn ir_flag_to_register(flag: X86Flag) -> Register {
    Rflag::from_ir_flag(flag).register()
}

/// Iterate over [`RflagsBits`] values. Only flags that we actually use are
/// included
fn iter_rflags_bits(value: u32) -> impl Iterator<Item = Rflag> {
    use Rflag::*;

    [OF, SF, ZF, CF]
        .into_iter()
        .filter(move |f| (value & (*f as u32)) != 0)
}

#[derive(Clone, Copy)]
enum Lvalue {
    Variable(Variable),
    Memory { addr: SimpleExpr, size: SizeBytes },
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

fn get_partial_regs(reg: X86Register) -> &'static [X86Register] {
    use X86Register::*;

    macro_rules! match_regs {
        (
            $reg:expr,
            $(($qword_reg:path, $dword_reg:path, $word_reg:path, $lo_reg:path $(, $hi_reg:path)?),)+
        ) => {
            match reg {
                $(
                    $word_reg => &[$lo_reg $(, $hi_reg)?],
                    $dword_reg => &[$lo_reg $(, $hi_reg)? , $word_reg],
                    $qword_reg => &[$lo_reg $(, $hi_reg)? , $word_reg, $dword_reg],
                )+

                _ => &[],
            }
        };
    }

    match_regs!(
        reg,
        (RAX, EAX, AX, AL, AH),
        (RCX, ECX, CX, CL, CH),
        (RDX, EDX, DX, DL, DH),
        (RBX, EBX, BX, BL, BH),
        (RSP, ESP, SP, SPL),
        (RBP, EBP, BP, BPL),
        (RSI, ESI, SI, SIL),
        (RDI, EDI, DI, DIL),
        (R8, R8D, R8W, R8L),
        (R9, R9D, R9W, R9L),
        (R10, R10D, R10W, R10L),
        (R11, R11D, R11W, R11L),
        (R12, R12D, R12W, R12L),
        (R13, R13D, R13W, R13L),
        (R14, R14D, R14W, R14L),
        (R15, R15D, R15W, R15L),
    )
}

fn get_full_regs(reg: X86Register) -> &'static [X86Register] {
    use X86Register::*;

    macro_rules! match_regs {
        (
            $reg:expr,
            $(($qword_reg:path, $dword_reg:path, $word_reg:path, $lo_reg:path $(, $hi_reg:path)?),)+
        ) => {
            match reg {
                $(
                    $dword_reg => &[$qword_reg],
                    $word_reg => &[$qword_reg, $dword_reg],
                    $lo_reg $(| $hi_reg)? => &[$qword_reg, $dword_reg, $word_reg],
                )+

                _ => &[],
            }
        };
    }

    match_regs!(
        reg,
        (RAX, EAX, AX, AL, AH),
        (RCX, ECX, CX, CL, CH),
        (RDX, EDX, DX, DL, DH),
        (RBX, EBX, BX, BL, BH),
        (RSP, ESP, SP, SPL),
        (RBP, EBP, BP, BPL),
        (RSI, ESI, SI, SIL),
        (RDI, EDI, DI, DIL),
        (R8, R8D, R8W, R8L),
        (R9, R9D, R9W, R9L),
        (R10, R10D, R10W, R10L),
        (R11, R11D, R11W, R11L),
        (R12, R12D, R12W, R12L),
        (R13, R13D, R13W, R13L),
        (R14, R14D, R14W, R14L),
        (R15, R15D, R15W, R15L),
    )
}

pub fn get_x86_register_size(x86_reg: X86Register) -> SizeBytes {
    SizeBytes(x86_reg.size().try_into().unwrap())
}

fn is_high_byte_reg(x86_reg: X86Register) -> bool {
    use X86Register::*;

    matches!(x86_reg, AH | BH | CH | DH)
}

fn get_partial_reg_shift(x86_reg: X86Register) -> u32 {
    if is_high_byte_reg(x86_reg) {
        8
    } else {
        0
    }
}
