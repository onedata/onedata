"""
Author: Piotr Ociepka
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements some common basic functions and functionality.
"""
import os

import pytest
from pytest_bdd import parsers
from pytest_bdd import when, then

####################### CLASSES #######################
import time


class Context:
    def __init__(self):
        self.users = {}


###################### FIXTURES  ######################

@pytest.fixture(scope="module")
def context(env_description_file):
    return Context()


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


