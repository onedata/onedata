"""Author: Piotr Ociepka
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements common functions authorization and mounting oneclient.
"""
import pytest
from pytest_bdd import (given, when, then)
from pytest_bdd import parsers

import subprocess
import os
import sys
from tempfile import SpooledTemporaryFile as tempfile
import time

from environment import docker, env


@given(parsers.parse('{user} mounts onedata spaces using {token}'))
def mount(user, token, environment, context):
    with open("/etc/resolv.conf", "w") as conf:
        dns = environment['dns']
        conf.write("nameserver " + dns)
    if token == "bad token":
        token = "bad_token"
    elif token == "token":
        gr_node = environment['gr_nodes'][0]
        token = subprocess.check_output(
            ['./tests/cucumber/scenarios/utils/get_token.escript', gr_node, "u1"],
            stderr=subprocess.STDOUT)

    print "TOKEN: " + token
    client = environment['client_nodes'][0]
    gr = environment['gr_nodes'][0]
    gr = gr.split('@')[1]

    Id = docker.inspect(client)['Id']
    mounting_result = docker.exec_(container=Id, command="mkdir ~/onedata &&\
            export GLOBAL_REGISTRY_URL=" + gr + \
            ' && echo ' + token + ' > token && ' +
            './oneclient --authentication token --no_check_certificate ~/onedata < token',
            stdout=sys.stdout,
            stderr=sys.stdout
            )
    # print mounting_result
    context.mounting_result = mounting_result


@then("mounting succeeds")
def success(context):
    assert context.mounting_result == 0


@then("mounting fails")
def failure(context):
    assert not context.mounting_result == 0


@then(parsers.parse('{spaces} are mounted'))
def check_spaces(environment, spaces):
    time.sleep(3)
    spaces_list = spaces.split(" ")
    client = environment['client_nodes'][0]
    Id = docker.inspect(client)['Id']
    spaces_in_client = docker.exec_(container=Id, command="ls ~/onedata/spaces", output=True)
    spaces_in_client = spaces_in_client.split(" ")
    assert spaces_list == spaces_in_client
    pass
