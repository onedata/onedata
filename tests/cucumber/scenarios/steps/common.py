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
        pass

###################### FIXTURES  ######################

@pytest.fixture(scope="module")
def context():
    return Context()


@pytest.fixture(scope="module")
def client_id(environment):
    client = environment['client_nodes'][0]
    return docker.inspect(client)['Id']

######################## STEPS ########################


@when(parsers.parse('sleep {time} seconds'))
def sleep(time, client_id):
    docker.exec_(container=client_id,
                 command="sleep " + str(time))


@when("last operation succeeds")
@then("last operation succeeds")
def success(context):
    assert context.last_op_ret_code == 0


@when("last operation fails")
@then("last operation fails")
def failure(context):
    assert context.last_op_ret_code != 0



###################### FUNCTIONS ######################

def list_parser(list):
    return [el.strip() for el in list.strip("[]").split(',')]

def save_op_code(context, op_code):
    context.last_op_ret_code = op_code

def make_path(context, path):
    return os.path.join(context.mount_path, str(path))
