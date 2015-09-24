"""
Author: Piotr Ociepka
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements pytest-bdd steps for authorization and mounting oneclient.
"""

import pytest
from pytest_bdd import (given, when, then)
from pytest_bdd import parsers

import subprocess
import sys
import time

from environment import docker, env
from common import *
from file_steps import clean

@given(parsers.parse('{user} mounts onedata spaces in {mount_path} using {token}'))
def mount(user, mount_path, token, environment, context, client_id):

    mount_path = str(mount_path)

    with open("/etc/resolv.conf", "w") as conf:
        dns = environment['dns']
        conf.write("nameserver " + dns)
    if token == "bad token":
        token = "bad_token"
    elif token == "token":
        gr_node = environment['gr_nodes'][0]
        token = subprocess.check_output(
            ['./tests/cucumber/scenarios/utils/get_token.escript', gr_node, user],
            stderr=subprocess.STDOUT)

    gr = environment['gr_nodes'][0]
    gr = gr.split('@')[1]

    cmd = "mkdir " + mount_path + " && export GLOBAL_REGISTRY_URL=" + gr + \
            ' && echo ' + token + ' > token && ' + \
            './oneclient --authentication token --no_check_certificate ' + mount_path + \
            ' < token && rm token'

    ret = docker.exec_(container=client_id, command=cmd, stdout=sys.stdout, stderr=sys.stdout)
    save_op_code(context, ret)

    context.mount_path = mount_path


@then(parsers.parse('{spaces} are mounted'))
def check_spaces(spaces, client_id, context):
    time.sleep(3)
    # sleep to be sure that environment is up
    spaces_list = list_parser(spaces)

    spaces_in_client = docker.exec_(container=client_id,
                                    command=['ls', context.mount_path + '/spaces'],
                                    output=True)
    spaces_in_client = spaces_in_client.split("\n")
    for space in spaces_list:
        assert space in spaces_in_client
