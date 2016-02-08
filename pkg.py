#!/usr/bin/env python

# coding=utf-8
"""Author: Tomasz Lichon
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Manage package repository.
"""
import argparse
from subprocess import Popen, PIPE, check_call, check_output, CalledProcessError
from glob import glob
import sys
import os

APACHE_PREFIX = '/var/www/onedata'
REPO_LOCATION = {
    'sid': '/apt/debian',
    'fedora-21-x86_64': '/yum/fedora/21',
    'fedora-23-x86_64': '/yum/fedora/23',
    'centos-7-x86_64': '/yum/centos/7x',
    'sl6x-x86_64': '/yum/scientific/6x'
}
REPO_TYPE = {
    'sid': 'deb',
    'fedora-21-x86_64': 'rpm',
    'fedora-23-x86_64': 'rpm',
    'centos-7-x86_64': 'rpm',
    'sl6x-x86_64': 'rpm'
}

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
    help='available distributions: sid, fedora-21-x86_64, fedora-23-x86_64, centos-7-x86_64, sl6x-x86_64',
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

# create the parser for the "push" command
parser_push = subparsers.add_parser(
    'push',
    help='Deploy .tar.gz package artifact.',
)
parser_push.add_argument(
    'targz',
    help='Packed onedata/package dir'
)

args = parser.parse_args()


def cp_or_scp(full_hostname, identity_opt, source, dest_dir):
    scp_command = ['scp'] + identity_opt + [source] + [full_hostname + ':' + dest_dir] \
        if not full_hostname.endswith('@localhost') else ['cp', source, dest_dir]
    check_call(scp_command, stdout=sys.stdout, stderr=sys.stderr)


def ssh_or_sh(full_hostname, identity_opt, command, return_output = False):
    ssh_command = ['ssh'] + identity_opt + [full_hostname] if args.host != 'localhost' else []
    if return_output:
        return check_output(ssh_command + command)
    else:
        return check_call(ssh_command + command, stdout=sys.stdout, stderr=sys.stderr)

def untar_remote_or_local(full_hostname, identity_opt, targz, dest_dir):
    ssh_command = ['ssh'] + identity_opt + [full_hostname] if args.host != 'localhost' else []
    tar_stream = Popen(['cat', targz], stdout=PIPE)
    check_call(ssh_command + ['tar', 'xzf', '-', '-C', dest_dir], stdin=tar_stream.stdout)
    tar_stream.wait()


identity_opt = ['-i', args.identity] if args.identity else []
full_hostname = args.user + '@' + args.host

copy = lambda source, dest_dir: cp_or_scp(full_hostname, identity_opt, source, dest_dir)
call = lambda command: ssh_or_sh(full_hostname, identity_opt, command, True)
execute = lambda command: ssh_or_sh(full_hostname, identity_opt, command)
untar = lambda targz, dest_dir: untar_remote_or_local(full_hostname, identity_opt, targz, dest_dir)

try:
    if args.action == 'push_deb':
        # copy to tmp
        copy(args.deb, '/tmp/')

        # update reprepro
        command = ['reprepro']
        command.extend(['-b', APACHE_PREFIX + REPO_LOCATION[args.distribution]])
        command.extend(['includedeb', args.distribution, '/tmp/' + os.path.basename(args.deb)])
        call(command)
    elif args.action == 'push_rpm':
        # copy to repo
        rpm_dir = APACHE_PREFIX + REPO_LOCATION[args.distribution] + '/x86_64/'
        srpm_dir = APACHE_PREFIX + REPO_LOCATION[args.distribution] + '/SRPMS/'
        copy(args.rpm, rpm_dir)
        copy(args.src_rpm, srpm_dir)

        # update createrepo
        repo_dir = APACHE_PREFIX + REPO_LOCATION[args.distribution]
        #todo enable + set gpg_check to 1 in onedata_devel.repo file
        # call(['rpm', '--resign'] + glob(repo_dir + '/**/*.rpm')])
        call(['createrepo', repo_dir])
    elif args.action == 'push':
        # extract package targz
        execute(['rm', '-rf', '/tmp/package'])
        untar(args.targz, '/tmp/')

        # for each distribution inside
        for distro in call(['ls', '/tmp/package']).split():
            if REPO_TYPE[distro] == 'deb':
                # prepare command
                repo_dir = APACHE_PREFIX + REPO_LOCATION[distro]
                command = ['reprepro', '-b', repo_dir]

                # add deb packages to reprepro
                distro_binary_prefix = '/tmp/package/' + distro + '/binary-amd64/'
                for package in call(['ls', distro_binary_prefix]).split():
                    if package.endswith('.deb'):
                        push_package_command = command + ['includedeb', distro, distro_binary_prefix + package]
                        execute(push_package_command)

                        #todo enable
                        # add dsc sources to reprepro
                        # distro_source_prefix = '/tmp/package/' + distro + '/source/'
                        # for source in call(['ls', distro_source_prefix]).split():
                        #     if source.endswith('.dsc'):
                        #         push_source_command = \
                        #             command + ['includedsc',distro, distro_source_prefix + source]
                        #         execute(push_source_command)
            elif REPO_TYPE[distro] == 'rpm':
                # copy packages
                repo_dir = APACHE_PREFIX + REPO_LOCATION[distro]
                distro_contents = '/tmp/package/' + distro + '/*'

                call(['cp','-R'] + glob(distro_contents) + [repo_dir])

                # update createrepo
                #todo enable + set gpg_check to 1 in onedata_devel.repo file
                # call(['rpm', '--resign'] + glob(repo_dir + '/**/*.rpm'))
                call(['createrepo', repo_dir])
except CalledProcessError as err:
    exit(err.returncode)
