import sys

import cle

import binq
from binq import Expr as E

from test_cle import lift_object


db = binq.Database("x64")
filename = sys.argv[1]
print("loading", filename, file=sys.stderr)
bin = cle.Loader(filename)
print("lifting", file=sys.stderr)
lift_object(db, bin.main_object)
db.print_il()
