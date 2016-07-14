#! /usr/bin/env python

import rpyc
import sys

CLIENT_IP = sys.argv[1]

conn = rpyc.classic.connect(CLIENT_IP)
for i in range(10):
    conn.modules.os.listdir("/home/u1/onedata/s1")
conn.close()
