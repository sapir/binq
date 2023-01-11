import sys

import cle

import binq
from binq import Expr as E
from binq.cle import lift_object


if __name__ == "__main__":
    db = binq.Database("x64")
    filename = sys.argv[1]
    print("loading", filename)
    bin = cle.Loader(filename)
    print("lifting")
    lift_object(db, bin.main_object)
    print("analyzing")
    db.analyze()
    print("done analyzing!")

    query = {"value": E.ANY + E(0x10)}
    for asm_addr, il_index in db.search(query):
        print(f"{asm_addr:#x}/{il_index}")
