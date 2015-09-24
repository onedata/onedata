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
                       command='echo "' + str(text) + '" > ' + make_path(context, file))
    save_op_code(context, ret)


@then(parsers.parse('{user} reads "{text}" from {file}'))
def read(user, text, file, context, client_id):
    read_text = docker.exec_(container=client_id,
                             command=["cat", make_path(context, file)],
                             output=True)
    assert read_text == text


@when(parsers.parse('{user} copies regular file {file} to {path}'))
def copy_reg_file(user, file, path, context, client_id):

    ret = docker.exec_(container=client_id,
                       command="cp " + make_path(context, file) +" " + make_path(context, path))
    save_op_code(context, ret)


@when(parsers.parse('{user} changes {file} size to {new_size} bytes'))
def truncate(user, file, new_size, context, client_id):
    ret = docker.exec_(container=client_id,
                       command=["truncate", "--size="+str(new_size),
                                make_path(context, file)])
    save_op_code(context, ret)
