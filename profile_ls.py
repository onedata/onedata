#! /usr/bin/env python

import rpyc
import sys
import subprocess
import os

CLIENT_IP = sys.argv[1]
RANGE = int(sys.argv[2])
CLIENT = sys.argv[3]
OP = sys.argv[4]
OZ = sys.argv[5]
PATH = "/home/u1/onedata/s1"
REMOUNT_SCRIPT = "./remount_client.sh"

conn = rpyc.classic.connect(CLIENT_IP)
for i in range(RANGE):
    subprocess.check_output([REMOUNT_SCRIPT, CLIENT, OP, OZ])
    dir = ""
    conn.modules.os.listdir(os.path.join(PATH, dir))
conn.close()
