#! /usr/bin/env python

import rpyc
import sys
import subprocess

CLIENT_IP = sys.argv[1]
CLIENT = sys.argv[2]
OP = sys.argv[3]
OZ = sys.argv[4]
PATH = "/home/u1/onedata/s1"
REMOUNT_SCRIPT = "./profiling/remount_client.sh"

conn = rpyc.classic.connect(CLIENT_IP)
subprocess.check_output([REMOUNT_SCRIPT, CLIENT, OP, OZ])
print conn.modules.os.listdir(PATH)
conn.close()
