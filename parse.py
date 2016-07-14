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

print "Parsing", PROFILE_LOG
print "No. of processes", len(sorted_pids)
with open(OUTPUT, 'a') as out:
    out.write("Listed processes number:{}\n".format(len(sorted_pids)))
    i = 1
    undefined_num = 0
    undefined_percentage_sum = 0
    for pid, percentage in sorted_pids:
        sys.stdout.write(str(i) + " ")
        sys.stdout.flush()

        cmd = [cmd_skeleton]
        cmd.extend([OP_NODE, pid])
        process_info = subprocess.check_output(cmd)
        if process_info == "undefined":
            undefined_num += 1
            undefined_percentage_sum += percentage
        out.write("######################################################################################\n"
                  "{0}: Process {1} took {2}% of time\n"
                  "--------------------------------------------------------------------------------------\n"
                  "{3}\n"
                  .format(i, pid, percentage, process_info))
        i += 1
    out.write("######################################################################################\n"
              "Number of undefined processes: {0}\n"
              "Undefined processes took {1}% of time in total\n"
              .format(undefined_num, undefined_percentage_sum))