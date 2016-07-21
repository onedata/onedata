#! /usr/bin/env python

import rpyc
import sys
import subprocess
import os


CLIENT_IP = sys.argv[1]
CLIENT = sys.argv[2]
OP = sys.argv[3]
OZ = sys.argv[4]
PATH = "/home/u1/onedata/s1"
REMOUNT_SCRIPT = "./profiling/remount_client.sh"

conn = rpyc.classic.connect(CLIENT_IP)
subprocess.check_output([REMOUNT_SCRIPT, CLIENT, OP, OZ])
file = os.path.join(PATH, "file")
conn.modules.os.mknod(file)
conn.close()
