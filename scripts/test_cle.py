import sys

from binq import Expr as E
from binq.cle import load


if __name__ == "__main__":
    db = load("x64", sys.argv[1])

    query = {"value": E.ANY.named("x") + E(0x10)}
    for m in db.search(query):
        asm_addr, il_index = m.pop("match_addr")
        print(f"{asm_addr:#x}/{il_index}: {m}")
