#!/usr/bin/env python

# coding=utf-8
"""Author: Tomasz Lichon
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Pushes .tar.gz package archives in onedata's bamboo artifact format:
i. e.
package/
    fedora-23-x86_64
        SRPMS
            cluster-manager-1.0.0.1.ge1a52f4-1.fc23.src.rpm
        x86_64
            cluster-manager-1.0.0.1.ge1a52f4-1.fc23.x86_64.rpm
    wily
        binary-amd64
            cluster-manager_1.0.0.1.ge1a52f4-1_amd64.deb
        source
            cluster-manager_1.0.0.1.ge1a52f4-1.diff.gz
            cluster-manager_1.0.0.1.ge1a52f4-1.dsc
            cluster-manager_1.0.0.1.ge1a52f4-1_amd64.changes
            cluster-manager_1.0.0.1.ge1a52f4.orig.tar.gz

Available distributions sid, wily, fedora-21-x86_64, fedora-23-x86_64, centos-7-x86_64, sl6x-x86_64
"""
import argparse
from subprocess import Popen, PIPE, check_call, check_output, CalledProcessError
import sys
import os

CONFIG = '''
Host docker_packages_devel
 HostName 172.17.0.2
 User root
 ProxyCommand ssh packages_devel nc %h %p

Host docker_packages
 HostName 172.17.0.2
 User root
 ProxyCommand ssh packages nc %h %p

Host packages_devel
 HostName 149.156.11.4
 Port 10107
 User ubuntu

Host packages
 HostName 149.156.11.4
 Port 10039
 User ubuntu
 '''

APACHE_PREFIX = '/var/www/onedata'
REPO_LOCATION = {
    'sid': '/apt/debian',
    'wily': '/apt/ubuntu',
    'fedora-21-x86_64': '/yum/fedora/21',
    'fedora-23-x86_64': '/yum/fedora/23',
    'centos-7-x86_64': '/yum/centos/7x',
    'sl6x-x86_64': '/yum/scientific/6x'
}
REPO_TYPE = {
    'sid': 'deb',
    'wily': 'deb',
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
    '--host',
    default=None,
    action='store',
    help='[user@]hostname to connect with package repo. In ssh format.',
    dest='host')

parser.add_argument(
    '-i', '--identity',
    default=None,
    action='store',
    help='Private key.',
    dest='identity')

# create the parser for the "config" command
parser_config = subparsers.add_parser(
        'config',
        help='Print ssh config for onedata package repositories'
)

# create the parser for the "push" command
parser_push = subparsers.add_parser(
    'push',
    help='Deploy .tar.gz package artifact.'
)
parser_push.add_argument(
    'package_artifact',
    help='Package artifact in tar.gz format'
)

args = parser.parse_args()


def cp_or_scp(hostname, identity_opt, source, dest_dir):
    scp_command = ['scp'] + identity_opt + [source] + [hostname + ':' + dest_dir] \
        if hostname else ['cp', source, dest_dir]
    check_call(scp_command, stdout=sys.stdout, stderr=sys.stderr)


def ssh_or_sh(hostname, identity_opt, command, return_output = False):
    ssh_command = ['ssh'] + identity_opt + [hostname] if hostname else []
    if return_output:
        return check_output(ssh_command + command)
    else:
        return check_call(ssh_command + command, stdout=sys.stdout, stderr=sys.stderr)


def untar_remote_or_local(hostname, identity_opt, package_artifact, dest_dir):
    ssh_command = ['ssh'] + identity_opt + [hostname] if hostname else []
    tar_stream = Popen(['cat', package_artifact], stdout=PIPE)
    check_call(ssh_command + ['tar', 'xzf', '-', '-C', dest_dir], stdin=tar_stream.stdout)
    tar_stream.wait()


identity_opt = ['-i', args.identity] if args.identity else []

copy = lambda source, dest_dir: cp_or_scp(args.host, identity_opt, source, dest_dir)
call = lambda command: ssh_or_sh(args.host, identity_opt, command, True)
execute = lambda command: ssh_or_sh(args.host, identity_opt, command)
untar = lambda package_artifact, dest_dir: untar_remote_or_local(args.host, identity_opt, package_artifact, dest_dir)

try:
    if args.action == 'config':
        print(CONFIG)
    elif args.action == 'push':
        # extract package_artifact
        execute(['rm', '-rf', '/tmp/package'])
        untar(args.package_artifact, '/tmp/')

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
                        try:
                            push_package_command = command + ['includedeb', distro, distro_binary_prefix + package]
                            execute(push_package_command)

                            # add dsc sources to reprepro
                            distro_source_prefix = '/tmp/package/' + distro + '/source/'
                            push_source_command = command + ['includedsc', distro, '{}']
                            call(['find', distro_source_prefix, '-name', '*.dsc', '-exec'] + push_source_command + ['\;'])
                        except CalledProcessError:
                            pass
            elif REPO_TYPE[distro] == 'rpm':
                # copy packages
                repo_dir = APACHE_PREFIX + REPO_LOCATION[distro]
                distro_contents = '/tmp/package/' + distro + '/.'

                call(['cp','-a', distro_contents, repo_dir])

                # update createrepo
                call(['find', repo_dir, '-name', '*.rpm', '-exec', 'rpmresign', '{}', '\';\''])
                call(['createrepo', repo_dir])
except CalledProcessError as err:
    exit(err.returncode)
