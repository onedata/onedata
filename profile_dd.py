#! /usr/bin/env python

import rpyc
import sys
import subprocess


CLIENT_IP = sys.argv[1]
RANGE = int(sys.argv[2])
CLIENT = sys.argv[3]
OP = sys.argv[4]
OZ = sys.argv[5]
PATH = "/home/u1/onedata/s1"
REMOUNT_SCRIPT = "./remount_client.sh"

conn = rpyc.classic.connect(CLIENT_IP)
subprocess.check_output([REMOUNT_SCRIPT, CLIENT, OP, OZ])
print conn.modules.subprocess.check_output(["dd", "if=/dev/zero", "of=/home/u1/onedata/s1/file", "bs=4K", "count=256"])

conn.close()