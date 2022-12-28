import sys
from pathlib import Path

import cle
from tqdm import tqdm

import binq
from binq import Expr as E


def lift_object(db, object: cle.Backend):
    buf = Path(object.binary).read_bytes()

    todo = {object.entry - object.mapped_base}
    todo.update(
        sym.linked_addr
        for sym in object.symbols
        if sym.is_function and sym.linked_addr != 0
    )
    todo = sorted(todo)

    for addr in tqdm(todo):
        db.add_func(object.linked_base, buf, addr)


db = binq.Database("x64")
filename = sys.argv[1]
print("loading", filename, file=sys.stderr)
bin = cle.Loader(filename)
print("lifting", file=sys.stderr)
lift_object(db, bin.main_object)
db.print_il()
