from pathlib import Path

import cle
from tqdm import tqdm

from . import Database


def lift_object(db, object: cle.Backend):
    buf = Path(object.binary).read_bytes()

    todo = {object.entry}
    for sym in object.symbols:
        if sym.is_function and not sym.is_import:
            addr = sym.rebased_addr

            section = object.find_section_containing(addr)
            if section is None:
                print("bad addr", hex(addr))
                continue

            if section.is_executable:
                todo.add(addr)

    todo = sorted(todo)

    for addr in tqdm(todo):
        section = object.find_section_containing(addr)
        section_buf = buf[section.offset : section.offset + section.filesize]
        db.add_func(section.vaddr, section_buf, addr)

def load(arch_and_abi, filename):
    db = Database(arch_and_abi)
    print("loading", filename)
    bin = cle.Loader(filename)
    print("lifting")
    lift_object(db, bin.main_object)
    print("analyzing")
    db.analyze()
    print("done analyzing!")
    return db
