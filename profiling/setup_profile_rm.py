#! /usr/bin/env python

import rpyc
import sys
import os

CLIENT_IP = sys.argv[1]
PATH = "/home/u1/onedata/s1"

conn = rpyc.classic.connect(CLIENT_IP)
dir_path=os.path.join(PATH, 'dir')
conn.modules.os.mkdir(dir_path)
for i in range(10):
    conn.modules.os.mkdir(os.path.join(dir_path, str(i)))
conn.close()
