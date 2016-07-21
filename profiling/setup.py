#! /usr/bin/env python

import rpyc
import sys
import os

CLIENT_IP = sys.argv[1]
PATH = "/home/u1/onedata/s1"

conn = rpyc.classic.connect(CLIENT_IP)
with conn.builtins.open(os.path.join(PATH, 'file'), 'w') as f:
    f.write("T")
    f.close()
conn.close()
