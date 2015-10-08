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

import os
from environment import docker

####################### CLASSES #######################
import time


class Context:
    def __init__(self):
        self.users = {}


class User:
    def __init__(self, client_node, client):
        self.clients = {client_node: client}
        self.last_op_ret_code = 0

class Client:
    def __init__(self, docker_id, mount_path):
        self.docker_id = docker_id
        self.mount_path = mount_path


###################### FIXTURES  ######################

@pytest.fixture(scope="module")
def context():
    return Context()


@pytest.fixture(scope="module")
def client_ids(environment):
    ids = []
    for client in environment['client_nodes']:
        ids.append(docker.inspect(client)['Id'])
    return ids


######################## STEPS ########################

@when(parsers.parse('{user} waits {time} seconds'))
def user_wait_default(user, time, context):
    user_wait(user, time, "client1", context)


@when(parsers.parse('{user} waits {time} seconds on {client_node}'))
def user_wait(user, time, client_node, context):
    client = get_client(client_node, user, context)
    run_cmd(user, client, "sleep " + str(time))


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

    if user != 'root' and isinstance(cmd, str):
        cmd = 'su -c "' + cmd + '" ' + str(user)
    elif user != 'root' and isinstance(cmd, list):
        cmd = ["su", "-c"] + cmd + [str(user)]

    return docker.exec_(container=client.docker_id, command=cmd, output=output)


def get_client(client_node, user, context):
    return context.users[user].clients[client_node]

