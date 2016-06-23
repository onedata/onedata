"""This module contains utility functions for running commands in docker
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from environment import docker

import subprocess


def run_cmd(user, client, cmd, detach=False, output=False, error=False):
    """Run command in docker
    :param user: command will be run as given user
    :param client: instance of utils.client_utils.Client class
    :param cmd: command to be run, can be string or list of strings
    :param detach: argument passed to docker.exec_, if true process started in
    docker will be in detached mode
    :param output: argument passed to docker.exec_, if false function will
    return exit code of run command, otherwise its output
    :param error: argument passed to docker.exec_, if true stderr will be
    redirected to stdout
    """
    # convert command into ascii string or list of ascii strings
    if isinstance(cmd, basestring):
        cmd = str(cmd)
    elif isinstance(cmd, list):
        cmd = [str(x) for x in cmd]

    if user != 'root' and isinstance(cmd, str):
        cmd = ['su', '-c', cmd, str(user)]
    elif user != 'root' and isinstance(cmd, list):
        cmd = ["su", "-c"] + cmd + [str(user)]

    return docker.exec_(container=client.docker_name, command=cmd, output=output,
                        tty=True, stderr=subprocess.STDOUT if error else None,
                        detach=detach)


def docker_ip(container):
    return docker.inspect(container)['NetworkSettings']['IPAddress']
