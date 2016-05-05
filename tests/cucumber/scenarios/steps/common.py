"""
Author: Piotr Ociepka
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements some common basic functions and functionality.
"""
import pytest
from pytest_bdd import given, when, then
from pytest_bdd import parsers

from tests.test_common import env_name
from environment import docker

import os

####################### CLASSES #######################
import time


class Context:
    def __init__(self):
        self.users = {}


class User:
    def __init__(self, client_node, client):
        self.clients = {client_node: client}
        self.last_op_ret_code = 0
        self.files = {}


class Client:
    def __init__(self, docker_id, mount_path):
        self.timeout = 0
        self.docker_id = docker_id
        self.mount_path = mount_path

    def set_timeout(self, timeout):
        self.timeout = timeout


class File:
    def __init__(self, ctime, mtime):
        self.ctime = ctime
        self.mtime = mtime

    def set_timestamps(self, ctime, mtime):
        self.ctime = ctime
        self.mtime = mtime


###################### FIXTURES  ######################

@pytest.fixture(scope="module")
def context(env_description_file):
    return Context()


@pytest.fixture(scope="module")
def client_ids(persistent_environment):
    ids = {}
    for client in persistent_environment['client_nodes']:
        client = str(client)
        client_name = client.split(".")[0]
        ids[client_name] = docker.inspect(client)['Id']
    return ids


@pytest.fixture(autouse=True)
def skip_by_env(request, env_description_file):
    """This function skips test cases decorated with:
    @pytest.mark.skip_env(*envs).
    Test won't start for each env in envs.
    If you want to skip whole module, you must define
    global variable in that module named pytestmark in
    the following way:
    pytestmark = pytest.mark.skip_env(*envs)
    """
    if request.node.get_marker('skip_env'):
        env = env_name(env_description_file)
        args = request.node.get_marker('skip_env').kwargs
        reason = args['reason']
        if env in args['envs']:
                pytest.skip('skipped on env: {env} with reason: {reason}'
                            .format(env=env, reason=reason))


@pytest.fixture(autouse=True)
def xfail_by_env(request, env_description_file):
    """This function marks test cases decorated with:
    @pytest.mark.skip_env(*envs)
    as expected to fail:
    Test will be marked as expected to fail for each
    env in envs.
    If you want to mark whole module, you must define
    global variable in that module named pytestmark in
    the following way:
    pytestmark = pytest.mark.xfail_env(*envs)
    """
    if request.node.get_marker('xfail_env'):
        env = env_name(env_description_file)
        args = request.node.get_marker('xfail_env').kwargs
        reason = args['reason']
        if env in args['envs']:
            pytest.xfail('xfailed on env: {env} with reason: {reason}'
                         .format(env=env, reason=reason))


######################## STEPS ########################

@when(parsers.parse('{user} waits {seconds} second'))
@then(parsers.parse('{user} waits {seconds} second'))
@when(parsers.parse('{user} waits {seconds} seconds'))
@then(parsers.parse('{user} waits {seconds} seconds'))
def user_wait_default(user, seconds):
    time.sleep(int(seconds))


@when(parsers.parse('last operation by {user} succeeds'))
@then(parsers.parse('last operation by {user} succeeds'))
def success(user, context):
    assert context.users[user].last_op_ret_code == 0


@when(parsers.parse('last operation by {user} fails'))
@then(parsers.parse('last operation by {user} fails'))
def failure(user, context):
    assert context.users[user].last_op_ret_code != 0


###################### FUNCTIONS ######################


def list_parser(list):
    return [el.strip() for el in list.strip("[]").split(',')]


def make_arg_list(arg):
    return "[" + arg + "]"


def save_op_code(context, user, op_code):
    context.users[user].last_op_ret_code = op_code


def make_path(path, client):
    return os.path.join(client.mount_path, str(path))


def run_cmd(user, client, cmd, output=False):
    # convert command into ascii string or list of ascii strings
    if isinstance(cmd, basestring):
        cmd = str(cmd)
    elif isinstance(cmd, list):
        cmd = [str(x) for x in cmd]

    if user != 'root' and isinstance(cmd, str):
        cmd = 'su -c "' + cmd + '" ' + str(user)
    elif user != 'root' and isinstance(cmd, list):
        cmd = ["su", "-c"] + cmd + [str(user)]

    return docker.exec_(container=client.docker_id, command=cmd, output=output, tty=True)


def get_client(client_node, user, context):
    return context.users[user].clients[client_node]


def repeat_until(condition, timeout=0):

    condition_satisfied = condition()
    while not condition_satisfied and timeout >= 0:
        print "TIMEOUT: ", timeout
        time.sleep(1)
        timeout -= 1
        condition_satisfied = condition()
    return timeout > 0 or condition_satisfied

# FILE SYSTEM OPERATIONS


def ls(client, user="root", path=".", output=True):
    cmd = "ls {path}".format(path=path)
    return run_cmd(user, client, cmd, output=output)


def mv(client, src, dest, user="root", output=False):
    cmd = "mv {src} {dest}".format(src=src, dest=dest)
    return run_cmd(user, client, cmd, output=output)


def chmod(client, mode, file, user="root", output=False):
    cmd = "chmod {mode} {file}".format(mode=mode, file=file)
    return run_cmd(user, client, cmd, output=output)


def stat(client, path, format=None, user="root", output=True):
    cmd = "stat {path} {format}".format(
            path=path,
            format="--format='{0}'".format(format) if format else "")
    return run_cmd(user, client, cmd, output=output)


def rm(client, path, recursive=False, force=False, user="root", output=False):
    cmd = "rm {recursive} {force} {path}".format(
            recursive="-r" if recursive else "",
            force="-f" if force else "",
            path=path)
    return run_cmd(user, client, cmd, output=output)


def rmdir(client, dir_path, recursive=False, from_path=None, user="root",
          output=False):
    cmd = ("{from_path}"
           "rmdir {recursive} {path}"
           ).format(
            from_path="cd {0} &&".format(from_path) if from_path else "",
            recursive="-p" if recursive else "",
            path=dir_path)
    return run_cmd(user, client, cmd, output=output)


def mkdir(client, dir_path, recursive=False, user="root", output=False):
    cmd = "mkdir {recursive} {path}".format(
            recursive="-p" if recursive else "",
            path=dir_path)
    return run_cmd(user, client, cmd, output=output)


def touch(client, file_path, user="root", output=False):
    cmd = "touch {path}".format(path=file_path)
    return run_cmd(user, client, cmd, output=output)


def cp(client, src, dest, recursive=False, user="root", output=False):
    cmd = "cp {recursive} {src} {dest}".format(
            recursive="-r" if recursive else "",
            src=src,
            dest=dest)
    return run_cmd(user, client, cmd, output=output)


def truncate(client, file_path, size, user="root", output=False):
    cmd = "truncate --size={size} {file_path}".format(size=size, file_path=file_path)
    return run_cmd(user, client, cmd, output=output)


def dd(client, block_size, count, output_file, input_file="/dev/zero",
       user="root", output=True):

    cmd = "dd {input} {output} {bs} {count}".format(
        input="if={}".format(input_file),
        output="of={}".format(output_file),
        bs="bs={}M".format(block_size),  # block_size is in MB
        count="count={}".format(count))
    return run_cmd(user, client, cmd, output=output)


def echo_to_file(client, text, file_path, new_line=False, escape=False,
                 user="root", overwrite=True, output=False):

    cmd = "echo {newline} {escape} '{text}' {redirect} {file_path}".format(
        newline="-n" if not new_line else "",
        escape="-e" if escape else "",
        text=text,
        redirect=">" if overwrite else ">>",
        file_path=file_path)
    return run_cmd(user, client, cmd, output=output)


def cat(client, file_path, user="root", output=True):
    cmd = "cat {file_path}".format(file_path=file_path)
    return run_cmd(user, client, cmd, output=output)


def md5sum(client, file_path, user="root", output=True):
    cmd = "md5sum {file_path}".format(file_path=file_path)
    return run_cmd(user, client, cmd, output=output)


def fusermount(client, path, user='root', unmount=False, lazy=False,
               quiet=False, output=False):
    cmd = "fusermount {unmount} {lazy} {quiet} {path}".format(
        unmount="-u" if unmount else "",
        lazy="-z" if lazy else "",
        quiet="-q" if quiet else "",
        path=path
    )
    return run_cmd(user, client, cmd, output)
