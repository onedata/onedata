#! /usr/bin/env python

import rpyc
import sys
import subprocess
import os


CLIENT_IP = sys.argv[1]
CLIENT = sys.argv[3]
OP = sys.argv[4]
OZ = sys.argv[5]
PATH = "/home/u1/onedata/s1"
REMOUNT_SCRIPT = "./remount_client.sh"

conn = rpyc.classic.connect(CLIENT_IP)
file = os.path.join(PATH, "file")
subprocess.check_output([REMOUNT_SCRIPT, CLIENT, OP, OZ])
with conn.builtins.open(file, 'r') as f:
        f.read()
conn.close()