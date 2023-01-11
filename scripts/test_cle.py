import sys

from binq import Expr as E
from binq.cle import load


if __name__ == "__main__":
    db = load("x64", sys.argv[1])

    query = {"value": E.ANY + E(0x10)}
    for asm_addr, il_index in db.search(query):
        print(f"{asm_addr:#x}/{il_index}")
