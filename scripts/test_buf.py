from archinfo import ArchAMD64
import pyvex

import binq

TEST_ADDR = 0x1000
TEST_CODE = b"\x48\xc7\xc0\x64\x00\x00\x00\x04\xc8\x48\x89\xc3\xc3"

irsb = pyvex.lift(TEST_CODE, TEST_ADDR, ArchAMD64())
db = binq.Database()
db.add_block(irsb)
