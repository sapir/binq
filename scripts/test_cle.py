import sys
from pathlib import Path

from archinfo import ArchAMD64
import pyvex
import cle

import binq


def lift_object_block(object, binary_buf, addr):
    print(f"lifting {addr:#x} to vex")
    return pyvex.IRSB(
        binary_buf, addr, object.arch, bytes_offset=object.addr_to_offset(addr)
    )


def lift_object(db, object):
    buf = Path(object.binary).read_bytes()
    # TODO: all blocks
    irsb = lift_object_block(object, buf, object.entry)
    print("adding block to db")
    db.add_block(irsb)


db = binq.Database()
filename = sys.argv[1]
print("loading", filename)
bin = cle.Loader(filename)
print("lifting")
lift_object(db, bin.main_object)
