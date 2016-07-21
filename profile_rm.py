#! /usr/bin/env python

import rpyc
import sys

CLIENT_IP = sys.argv[1]
PATH = "/home/u1/onedata/s1/0"


conn = rpyc.classic.connect(CLIENT_IP)
conn.modules.shutil.rmtree(PATH, ignore_errors=True)
conn.close()
