#!/usr/bin/env python

# coding=utf-8
"""Author: Tomasz Lichon
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Manage package repository.
"""
import argparse
import subprocess
import sys
import os

APACHE_PREFIX = '/var/www/onedata'
REPO_LOCATION = {'sid': '/apt/debian', 'fc21': '/yum/fedora/21'}


# create the top-level parser
parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Manage package repository.')
subparsers = parser.add_subparsers(
    help='Available actions',
    dest='action'
)

parser.add_argument(
    '-u', '--user',
    default='root',
    action='store',
    help='Username from package repository.',
    dest='user')

parser.add_argument(
    '--host',
    default='localhost',
    action='store',
    help='Package repository hostname.',
    dest='host')

parser.add_argument(
    '-i', '--identity',
    default=None,
    action='store',
    help='Private key.',
    dest='identity')

# create the parser for the "push_deb" command
parser_push_deb = subparsers.add_parser(
    'push_deb',
    help='Deploy package to apt repository.',
)
parser_push_deb.add_argument(
    'distribution',
    help='available distributions: sid',
)
parser_push_deb.add_argument(
    'deb',
    help='Package to deploy'
)

# create the parser for the "push_rpm" command
parser_push_rpm = subparsers.add_parser(
    'push_rpm',
    help='Deploy package to yum repository.',
)
parser_push_rpm.add_argument(
    'distribution',
    help='Available distributions: fc21.',
)
parser_push_rpm.add_argument(
    'rpm',
    help='Package to deploy.'
)
parser_push_rpm.add_argument(
    'src_rpm',
    help='Source package to deploy.'
)


def cp_or_scp(full_hostname, identity_opt, filename, prefix):
    scp_command = ['scp'] + identity_opt + [filename] + [full_hostname + ':' + prefix] \
        if not full_hostname.endswith('@localhost') else ['cp', args.deb, prefix]
    subprocess.check_call(scp_command, stdout=sys.stdout, stderr=sys.stderr)


def ssh_or_sh_command(full_hostname, identity_opt, command):
    ssh_command = ['ssh'] + identity_opt + [full_hostname] if args.host != 'localhost' else []
    subprocess.check_call(ssh_command + command, stdout=sys.stdout, stderr=sys.stderr)

args = parser.parse_args()

identity_opt = ['-i', args.identity] if args.identity else []
full_hostname = args.user + '@' + args.host

if args.action == 'push_deb':
    try:
        # copy to tmp
        cp_or_scp(full_hostname, identity_opt, args.deb, "/tmp/")

        # update reprepro
        command = ['reprepro']
        command.extend(['-b', APACHE_PREFIX + REPO_LOCATION[args.distribution]])
        command.extend(['includedeb', args.distribution, '/tmp/' + os.path.basename(args.deb)])
        ssh_or_sh_command(full_hostname, identity_opt, command)
    except subprocess.CalledProcessError as err:
        exit(err.returncode)
elif args.action == 'push_rpm':
    try:
        # copy to repo
        rpm_dir = APACHE_PREFIX + REPO_LOCATION[args.distribution] + '/x86_64/'
        srpm_dir = APACHE_PREFIX + REPO_LOCATION[args.distribution] + '/SRPMS/'
        cp_or_scp(full_hostname, identity_opt, args.rpm, rpm_dir)
        cp_or_scp(full_hostname, identity_opt, args.src_rpm, srpm_dir)

        # update createrepo
        repo_dir = APACHE_PREFIX + REPO_LOCATION[args.distribution]
        command = ['createrepo', repo_dir]
        ssh_or_sh_command(full_hostname, identity_opt, command)
    except subprocess.CalledProcessError as err:
        exit(err.returncode)
