"""
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements pytest-bdd steps for operations on regular files.
"""

import pytest
from pytest_bdd import (given, when, then)
from pytest_bdd import parsers
import time

from environment import docker, env
from common import *


@when(parsers.parse('{user} writes "{text}" to {file}'))
def write(user, text, file, context, client_id):
    ret = docker.exec_(container=client_id,
                       command='echo "' + str(text) + '" > ' + str('/'.join([context.mount_path, file])))
    save_op_code(context, ret)


@then(parsers.parse('{user} reads "{text}" from {file}'))
def read(user, text, file, context, client_id):
    read_text = docker.exec_(container=client_id,
                             command=["cat", str('/'.join([context.mount_path, file]))],
                             output=True)
    assert read_text == text


@when(parsers.parse('{user} copies regular file {file} to {path}'))
def copy_reg_file(user, file, path, context, client_id):

    ret = docker.exec_(container=client_id,
                       command="cp " + str('/'.join([context.mount_path, file])) + \
                        " " + str('/'.join([context.mount_path, path])))
    save_op_code(context, ret)


@when(parsers.parse('{user} changes {file} size to {new_size} bytes'))
def truncate(user, file, new_size, context, client_id):
    ret = docker.exec_(container=client_id,
                       command=["truncate", "--size="+str(new_size),
                                '/'.join([context.mount_path, file])])
    save_op_code(context, ret)
