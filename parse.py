#! /usr/bin/env python

import re
import sys
import os
from operator import itemgetter
import subprocess

ESCRIPT_NAME = "find_name.escript"

if len(sys.argv) < 4:
    print "Usage: parse.py <OpNode> <ProfileLog> <Output>"
    sys.exit(1)

OP_NODE = sys.argv[1]
PROFILE_LOG = sys.argv[2]
OUTPUT = sys.argv[3]


REGEX = r'\*+\s+Process\s+(.*)\s+\-\- (\d+\.\d+)\s% of profiled time \*+'
PATTERN = re.compile(REGEX)

pids = []

with open(PROFILE_LOG, 'r') as f:
    for line in f:
        m = re.match(PATTERN, line)
        if m:
            pids.append((m.group(1).strip(), float(m.group(2).strip())))

sorted_pids = sorted(pids, key=itemgetter(1), reverse=True)

cmd_skeleton = "{cwd}/{escript}".format(cwd=os.getcwd(), escript=ESCRIPT_NAME)

print "Listed processes number: ", len(sorted_pids)

with open(OUTPUT, 'a') as out:
    i = 1
    for pid, percentage in sorted_pids:
        cmd = [cmd_skeleton]
        cmd.extend([OP_NODE, pid])
        registered_name = subprocess.check_output(cmd)
        # print "REGISTERED NAME", registered_name, registered_name.__class__, pid
        out.write("{0}: Process {1} with pid {2} took {3}% of time\n"\
            .format(i, registered_name, pid, percentage))
        i += 1
