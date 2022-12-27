import binq
from binq import Expr as E

TEST_ADDR = 0x1000
TEST_CODE = b"\x48\xc7\xc0\x64\x00\x00\x00\x04\xc8\x48\x89\xc3\xc3"

db = binq.Database("x64")
db.add_func(TEST_ADDR, TEST_CODE)
db.analyze()

for asm_addr, il_index in db.search({"value": E.ANY + E(0x10)}):
    print(f"{asm_addr:#x}/{il_index}")
