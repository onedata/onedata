#! /usr/bin/env python
import subprocess

import rpyc
import sys
import os


CLIENT_IP = sys.argv[1]
CLIENT = sys.argv[2]
OP = sys.argv[3]
OZ = sys.argv[4]
PATH = "/home/u1/onedata/s1"
REMOUNT_SCRIPT = "./profiling/remount_client.sh"

conn = rpyc.classic.connect(CLIENT_IP)
subprocess.check_output([REMOUNT_SCRIPT, CLIENT, OP, OZ])
with conn.builtins.open(os.path.join(PATH, 'file'), 'w') as f:
    f.write("T")
    f.close()
conn.close()
