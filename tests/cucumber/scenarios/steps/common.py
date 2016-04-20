"""
Author: Piotr Ociepka
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements some common basic functions and functionality.
"""
from tests.test_common import env_name

import pytest
from pytest_bdd import given, when, then
from pytest_bdd import parsers

import os
from environment import docker

####################### CLASSES #######################
import time


class Context:
    def __init__(self):
        self.users = {}

    def update_timestamps(self, user, client, file):
        """
        Updates mtime and ctime of file in user's test context.
        This timestamps are checked by user in functions:
        user_wait_ctime() and user_wait_mtime() and compared
        with actual timestamps read from stat
        """
        times = run_cmd(user, client, "stat --format=%Y,%Z " + make_path(file, client),
                        output=True).split(",")

        mtime = times[0]
        ctime = times[1]

        if not file in self.users[user].files.keys():
            self.users[user].files[file] = File(mtime, ctime)
        else:
            self.users[user].files[file].set_timestamps(mtime, ctime)

    def get_last_mtime(self, user, file):
        self.get_last_time(user, str(file), "mtime")

    def get_last_ctime(self, user, file):
        self.get_last_time(user, str(file), "ctime")

    def get_last_time(self, user, file, time_type):
        if file not in self.users[user].files.keys():
            return 0
        return getattr(self.users[user].files[file], time_type)


class User:
    def __init__(self, client_node, client):
        self.clients = {client_node: client}
        self.last_op_ret_code = 0
        self.files = {}


class Client:
    def __init__(self, docker_id, mount_path):
        self.docker_id = docker_id
        self.mount_path = mount_path


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


######################## STEPS ########################

@when(parsers.parse('{user} waits {seconds} second'))
@then(parsers.parse('{user} waits {seconds} second'))
@when(parsers.parse('{user} waits {seconds} seconds'))
@then(parsers.parse('{user} waits {seconds} seconds'))
def user_wait_default(user, seconds):
    time.sleep(int(seconds))


@when(parsers.parse('{user} waits up to {seconds} seconds for environment synchronization'))
@then(parsers.parse('{user} waits up to {seconds} seconds for environment synchronization'))
@given(parsers.parse('{user} waits up to {seconds} seconds for environment synchronization'))
def user_wait_up_to(user, seconds, context, env_description_file):
    # TODO This is just a temporary solution, delete it after resolving VFS-1881
    if env_name(env_description_file) not in ['env', 'env2']:
        time.sleep(int(seconds))
    else:
        time.sleep(1)


@when(parsers.parse('{user} waits max {maxtime} seconds for change of {file} mtime'))
def user_wait_mtime(user, maxtime, file, context):
    user_wait_mtime(user, maxtime, file, "client1", context)


@when(parsers.parse('{user} waits max {maxtime} seconds for change of {file} mtime on {client_node}'))
def user_wait_mtime(user, maxtime, file, client_node, context):
    client = get_client(client_node, user, context)
    waited = 0
    # compare mtime read from stat with last mtime saved by user
    # inequality of these timestamps means that other client
    # performed operation on the file
    while get_current_mtime(user, file, client) == context.get_last_mtime(user, file):
        time.sleep(1)
        waited += 1
        if waited >= maxtime:
            assert False


@when(parsers.parse('{user} waits max {maxtime} seconds for change of {file} ctime'))
def user_wait_ctime(user, maxtime, file, context):
    user_wait_ctime(user, maxtime, file, "client1", context)


@when(parsers.parse('{user} waits max {maxtime} seconds for change of {file} ctime on {client_node}'))
def user_wait_ctime(user, maxtime, file, client_node, context):
    client = get_client(client_node, user, context)
    waited = 0
    # compare ctime read from stat with last ctime saved by user
    # inequality of these timestamps means that other client
    # performed operation on the file
    while get_current_ctime(user, file, client) == context.get_last_ctime(user, file):
        time.sleep(1)
        waited += 1
        if waited >= maxtime:
            assert False


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


def get_current_mtime(user, file, client):
    get_current_time(user, str(file), client, "mtime")


def get_current_ctime(user, file, client):
    get_current_time(user, str(file), client, "ctime")


def get_current_time(user, file, client, time_type):
    opt = "Y" if time_type == "mtime" else "Z"
    return run_cmd(user, client, "stat --format=%" + opt + ' ' + make_path(file, client),
                   output=True)


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
