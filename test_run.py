#!/usr/bin/env python

"""
Runs integration and acceptance tests in docker environment.

All paths used are relative to script's path, not to the running user's CWD.
Run the script with -h flag to learn about script's running options.
"""

import argparse
import os
import platform
import sys
import time

sys.path.insert(0, 'bamboos/docker')
import docker

parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Run Common Tests.')

parser.add_argument(
    '--image', '-i',
    action='store',
    default='onedata/worker',
    help='docker image to use as a test master',
    dest='image')

parser.add_argument(
    '--suite', '-s',
    action='append',
    help='name of the test suite',
    dest='suites')

parser.add_argument(
    '--case', '-c',
    action='append',
    help='name of the test case',
    dest='cases')

args = parser.parse_args()
script_dir = os.path.dirname(os.path.realpath(__file__))
uid = str(int(time.time()))

command = '''
import os, subprocess, sys, stat

if {shed_privileges}:
    os.environ['HOME'] = '/tmp'
    docker_gid = os.stat('/var/run/docker.sock').st_gid
    os.chmod('/etc/resolv.conf', 0o666)
    os.setgroups([docker_gid])
    os.setregid({gid}, {gid})
    os.setreuid({uid}, {uid})

command = ['py.test', 'tests/env_tests']
ret = subprocess.call(command)
sys.exit(0)
'''
command = command.format(
    uid=os.geteuid(),
    gid=os.getegid(),
    shed_privileges=(platform.system() == 'Linux'))

ret = docker.run(tty=True,
                 rm=True,
                 interactive=True,
                 detach=False,
                 workdir=script_dir,
                 reflect=[(script_dir, 'rw'),
                          ('/var/run/docker.sock', 'rw')],
                 name='testmaster_{0}'.format(uid),
                 hostname='testmaster.{0}.dev.docker'.format(uid),
                 image=args.image,
                 command=['python', '-c', command])
sys.exit(ret)
