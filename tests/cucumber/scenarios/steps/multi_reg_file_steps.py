"""
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements pytest-bdd steps for operations on regular files.
"""
import subprocess

import pytest
from pytest_bdd import (given, when, then)
from pytest_bdd import parsers
import time
import string
import random
from os import *

from environment import docker, env
from common import *


@when(parsers.parse('{user} writes {megabytes} MB of random characters to {file} on {client_node}\n'+
                    'and saves MD5'))
def write_rand_text(user, megabytes, file, client_node, context):
    client = get_client(client_node, user, context)
    path = make_path(file, client)
    run_cmd(client, "dd if=/dev/urandom of=" + path + " bs=" + megabytes+"M + count=1")
    md5 = run_cmd(client, "md5sum " + path, output=True)
    context.md5 = md5.split()[0]


@when(parsers.parse('{user} writes "{text}" to {file} on {client_node}'))
def write_text(user, text, file, client_node, context):
    client = get_client(client_node, user, context)
    ret = run_cmd(client, 'echo "' + str(text) + '" > ' + make_path(file, client))
    save_op_code(context, user, ret)


@then(parsers.parse('{user} reads "{text}" from {file} on {client_node}'))
def read(user, text, file, client_node, context):
    client = get_client(client_node, user, context)
    read_text = run_cmd(client, "cat " + make_path(file, client), output=True)
    assert read_text == text


@then(parsers.parse('{user} checks MD5 of {file} on {client_node}'))
def check_md5(user, file, client_node, context):
    client = get_client(client_node, user, context)
    md5 = run_cmd(client, "md5sum " + make_path(file, client), output=True)
    assert md5.split()[0] == context.md5


@when(parsers.parse('{user} copies regular file {file} to {path} on {client_node}'))
def copy_reg_file(user, file, path, client_node, context):
    client = get_client(client_node, user, context)
    ret = run_cmd(client, "cp " + make_path(file, client) + " " + make_path(path, client))
    save_op_code(context, user, ret)


@when(parsers.parse('{user} changes {file} size to {new_size} bytes on {client_node}'))
def truncate(user, file, new_size, client_node, context):
    client = get_client(client_node, user, context)
    ret = run_cmd(client, ["truncate", "--size=" + str(new_size), make_path(file, client)])
    save_op_code(context, user, ret)
