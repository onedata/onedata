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
import multi_reg_file_steps


@when(parsers.parse('{user} writes {megabytes} MB of random characters to {file} and saves MD5'))
def write_rand_text(user, megabytes, file, context):
    multi_reg_file_steps.write_rand_text(user, megabytes, file, "client1", context)


@when(parsers.parse('{user} writes "{text}" to {file}'))
def write_text(user, text, file, context):
    multi_reg_file_steps.write_text(user, text, file, "client1", context)


@then(parsers.parse('{user} reads "{text}" from {file}'))
def read(user, text, file, context):
    multi_reg_file_steps.read(user, text, file, "client1", context)


@then(parsers.parse('{user} checks MD5 of {file}'))
def check_md5(user, file, context):
    multi_reg_file_steps.check_md5(user, file, "client1", context)


@when(parsers.parse('{user} copies regular file {file} to {path}'))
def copy_reg_file(user, file, path, context):
    multi_reg_file_steps.copy_reg_file(user, file, path, "client1", context)


@when(parsers.parse('{user} changes {file} size to {new_size} bytes'))
def truncate(user, file, new_size, context):
    multi_reg_file_steps.truncate(user, file, new_size, "client1", context)
