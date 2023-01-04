use std::{
    io::{prelude::*, BufReader},
    process::Command,
};

use itertools::Itertools;

use crate::{
    analysis::analyze,
    database::{ArchAndAbi, Database, StatementAddr},
    ir::{Addr64, CompareOpKind},
    print_il,
    query::{search, ConditionExpr, Expr, ExprMatchFilter, Field},
};

const BASE_ADDR: Addr64 = 0x1000;

fn run(command: &mut Command) {
    let status = command
        .status()
        .unwrap_or_else(|err| panic!("failed to run {:?}: {}", command, err));

    if !status.success() {
        panic!("'{:?}' returned an error ({:?})", command, status.code());
    }
}

fn assemble(bitness: u32, code: &str) -> Vec<u8> {
    let src_file = tempfile::Builder::new().suffix(".s").tempfile().unwrap();
    let obj_file = tempfile::Builder::new().suffix(".o").tempfile().unwrap();
    let bin_file = tempfile::Builder::new().suffix(".bin").tempfile().unwrap();

    {
        let mut src_file = src_file.as_file();
        src_file
            .write_all(
                concat!(
                    ".intel_syntax\n",
                    ".text\n",
                    ".global main\n",
                    ".type main, @function\n",
                    "main:\n",
                )
                .as_bytes(),
            )
            .unwrap();
        src_file.write_all(code.as_bytes()).unwrap();
        if !code.ends_with("\n") {
            src_file.write_all(b"\n").unwrap();
        }
        src_file.flush().unwrap();
    }

    run(Command::new("cc")
        .arg(&format!("-m{bitness}"))
        .arg("-x")
        .arg("assembler")
        .arg("-c")
        .arg(src_file.path())
        .arg("-o")
        .arg(obj_file.path()));

    run(Command::new("objcopy")
        .arg("-j")
        .arg(".text")
        .arg(obj_file.path())
        .arg("-O")
        .arg("binary")
        .arg(bin_file.path()));

    let mut buf = vec![];
    BufReader::new(bin_file).read_to_end(&mut buf).unwrap();
    buf
}

// Test the testing infrastructure
#[test]
fn assemble_works() {
    assert_eq!(
        assemble(
            32,
            "
            mov %eax, 100
            ret
            "
        ),
        b"\xb8\x64\x00\x00\x00\xc3"
    );
    assert_eq!(
        assemble(
            64,
            "
            mov %rax, 100
            ret
            "
        ),
        b"\x48\xc7\xc0\x64\x00\x00\x00\xc3"
    );
}

fn assemble_to_db(arch_and_abi: ArchAndAbi, code: &str) -> Database {
    let buf = assemble(arch_and_abi.arch().bitness(), code);

    let mut db = Database::new(arch_and_abi);
    db.add_buf(0x1000, &buf).unwrap();
    analyze(&mut db);
    print_il(&mut db);
    db
}

fn asm_addrs(stmt_addrs: impl IntoIterator<Item = StatementAddr>) -> Vec<Addr64> {
    stmt_addrs
        .into_iter()
        .map(|stmt_addr| stmt_addr.asm_addr)
        .unique()
        .collect()
}

macro_rules! assert_match_at {
    ($mut_db:expr, $field:expr, $expr:expr, $addr:expr) => {
        assert_eq!(
            asm_addrs(search(
                $mut_db,
                &[ExprMatchFilter {
                    field: $field,
                    expr: $expr
                }]
            )),
            [$addr],
            "{:x?} should have matched exactly once @ {:#x} but it didn't",
            $expr,
            $addr,
        );
    };

    ($arch_and_abi:expr, $code:expr, $field:expr, $expr:expr, $addr:expr) => {
        let mut db = assemble_to_db($arch_and_abi, $code);
        assert_match_at!(&mut db, $field, $expr, $addr);
    };
}

macro_rules! assert_match {
    ($mut_db:expr, $field:expr, $expr:expr) => {
        assert_match_at!($mut_db, $field, $expr, BASE_ADDR);
    };

    ($arch_and_abi:expr, $code:expr, $field:expr, $expr:expr) => {
        assert_match_at!($arch_and_abi, $code, $field, $expr, BASE_ADDR);
    };
}

macro_rules! assert_no_match {
    ($mut_db:expr, $field:expr, $expr:expr) => {
        assert_eq!(
            asm_addrs(search(
                $mut_db,
                &[ExprMatchFilter {
                    field: $field,
                    expr: $expr
                }]
            )),
            [],
            "{:x?} shouldn't have matched at all, but it did",
            $expr,
        );
    };

    ($arch_and_abi:expr, $code:expr, $field:expr, $expr:expr) => {
        let mut db = assemble_to_db($arch_and_abi, $code);
        assert_no_match!(&mut db, $field, $expr);
    };
}

#[test]
fn match_const() {
    assert_match!(
        ArchAndAbi::X64,
        "mov %rax, 100",
        Field::Value,
        Expr::AnyConst
    );
    assert_match!(
        ArchAndAbi::X64,
        "mov %rax, 100",
        Field::Value,
        Expr::Const(100)
    );
    assert_no_match!(
        ArchAndAbi::X64,
        "mov %rax, 100",
        Field::Value,
        Expr::Const(99)
    );
}

#[test]
fn match_const_in_partial_register_simple() {
    assert_match!(ArchAndAbi::X64, "mov %al, 5", Field::Value, Expr::AnyConst);
    assert_match!(ArchAndAbi::X64, "mov %ah, 5", Field::Value, Expr::AnyConst);

    assert_match!(ArchAndAbi::X64, "mov %al, 5", Field::Value, Expr::Const(5));
    assert_match!(ArchAndAbi::X64, "mov %ah, 5", Field::Value, Expr::Const(5));
}

#[test]
fn match_const_in_partial_register_extending() {
    // mov %al, -1 should match the 8-bit -1, but also the 64-bit -1 - even in
    // 32-bit mode (because the pattern is 64-bit anyway).

    // Match 8-bit -1 value with 0xff
    assert_match!(
        ArchAndAbi::X64,
        "mov %al, -1",
        Field::Value,
        Expr::Const(0xff)
    );

    // Match 8-bit -1 value with AnyConst, too.
    assert_match!(ArchAndAbi::X64, "mov %al, -1", Field::Value, Expr::AnyConst);

    // Match 8-bit -1 value with a 64-bit -1 pattern, even in 32-bit mode
    assert_match!(
        ArchAndAbi::X64,
        "mov %al, -1",
        Field::Value,
        Expr::Const(u64::MAX)
    );
    assert_match!(
        ArchAndAbi::X86,
        "mov %al, -1",
        Field::Value,
        Expr::Const(u64::MAX)
    );
}

#[test]
fn match_two_part_consts() {
    assert_match_at!(
        ArchAndAbi::X64,
        "
        mov %al, 1
        mov %ah, 1
        ",
        Field::Value,
        Expr::Const(0x101),
        BASE_ADDR + 2
    );
    assert_match_at!(
        ArchAndAbi::X64,
        "
        mov %ah, 1
        mov %al, 1
        ",
        Field::Value,
        Expr::Const(0x101),
        BASE_ADDR + 2
    );

    assert_match_at!(
        ArchAndAbi::X64,
        "
        mov %rax, 0x10000
        mov %al, 1
        ",
        Field::Value,
        Expr::Const(0x10001),
        BASE_ADDR + 7
    );
    assert_match_at!(
        ArchAndAbi::X64,
        "
        mov %rax, 0x10000
        mov %ah, 1
        ",
        Field::Value,
        Expr::Const(0x10100),
        BASE_ADDR + 7
    );

    // "mov %eax, 0x10000 ; mov %al, 1" should match 0x10001, both in 32- and
    // 64-bit mode
    assert_match_at!(
        ArchAndAbi::X86,
        "
        mov %eax, 0x10000
        mov %al, 1
        ",
        Field::Value,
        Expr::Const(0x10001),
        BASE_ADDR + 5
    );
    assert_match_at!(
        ArchAndAbi::X64,
        "
        mov %eax, 0x10000
        mov %al, 1
        ",
        Field::Value,
        Expr::Const(0x10001),
        BASE_ADDR + 5
    );
}

#[test]
fn match_const_in_partial_reg_condition() {
    assert_match_at!(
        ArchAndAbi::X64,
        "
        mov %rax, 1
        test %al, %al
        jne 0x1200
        ",
        Field::Condition,
        Expr::Condition(Box::new(ConditionExpr {
            kind: CompareOpKind::NotEqual,
            lhs: Expr::Const(1),
            rhs: Expr::Const(0),
        })),
        BASE_ADDR + 9
    );
}
