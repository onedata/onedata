"""Author: Piotr Ociepka
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements some common basic functions and functionality.
"""

import pytest
from pytest_bdd import then

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

@then("last operation succeeds")
def success(client_id, context):
    # nie dziala, bo exec sa w innych terminalach
    # assert docker.exec_(container=client_id, command="echo $?", output=True) == '0'
    assert context.last_op_ret_code == 0


@then("last operation fails")
def failure(client_id, context):
    # time.sleep(600)
    # retCode = docker.exec_(container=client_id, command="echo $?", output=True)
    # print "RET: " + retCode
    # assert retCode != '0'
    assert context.last_op_ret_code != 0

###################### FUNCTIONS ######################

def list_parser(list):
    return [el.strip() for el in list.strip("[]").split(',')]

def save_op_code(context, op_code):
    context.last_op_ret_code = op_code

