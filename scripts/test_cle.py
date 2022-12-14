from contextlib import closing
import sys
from pathlib import Path

from archinfo import ArchAMD64
import pyvex
import cle
from tqdm import tqdm

import binq


def lift_object_block(object, binary_buf, addr):
    """Returns None if addr_to_offset fails"""

    offset = object.addr_to_offset(addr)
    if offset is None:
        return None

    return pyvex.IRSB(binary_buf, addr, object.arch, bytes_offset=offset)


def lift_object(db, object):
    buf = Path(object.binary).read_bytes()

    todo = [object.entry]
    todo.extend(sym.rebased_addr for sym in object.symbols if sym.is_function)
    # todo.extend(fn.addr for fn in object.function_hints)
    done = set()

    with closing(tqdm(total=len(buf), unit="B", unit_scale=True)) as progress:
        while todo:
            addr = todo.pop()
            if addr in done:
                continue

            done.add(addr)

            irsb = lift_object_block(object, buf, addr)
            if irsb is None:
                continue

            db.add_block(irsb)

            for stmt in irsb.statements:
                tag = stmt.tag
                if tag == "Ist_Exit":
                    todo.append(stmt.dst.value)

            try:
                if irsb.jumpkind != "Ijk_NoDecode":
                    next_ = irsb.next.con.value
                    todo.append(next_)
            except AttributeError:
                pass

            progress.update(irsb.size)


db = binq.Database()
filename = sys.argv[1]
print("loading", filename)
bin = cle.Loader(filename)
print("lifting")
lift_object(db, bin.main_object)
db.analyze()
