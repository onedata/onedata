#! /usr/bin/env python

import rpyc
import sys
import os

CLIENT_IP = sys.argv[1]
RANGE = int(sys.argv[2])
PATH = "/home/u1/onedata/s1"


conn = rpyc.classic.connect(CLIENT_IP)
for i in range(RANGE):
    conn.modules.os.mkdir(os.path.join(PATH, str(i)))
conn.close()
