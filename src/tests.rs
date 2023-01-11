use std::{
    io::{prelude::*, BufReader},
    process::Command,
};

use itertools::Itertools;

use crate::{
    analysis::analyze,
    database::{ArchAndAbi, Database, StatementAddr},
    ir::Addr64,
    print_il,
    query::{search, Expr, ExprMatchFilter, Field},
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

macro_rules! assert_match {
    ($mut_db:expr, $field:expr, $expr:expr) => {
        assert_eq!(
            asm_addrs(search(
                $mut_db,
                &[ExprMatchFilter {
                    field: $field,
                    expr: $expr
                }]
            )),
            [BASE_ADDR],
            "{:x?} should have matched exactly once @ {:#x} but it didn't",
            $expr,
            BASE_ADDR,
        );
    };

    ($arch_and_abi:expr, $code:expr, $field:expr, $expr:expr) => {
        let mut db = assemble_to_db($arch_and_abi, $code);
        assert_match!(&mut db, $field, $expr);
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
