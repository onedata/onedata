"""Module implements pytest-bdd steps for operations on regular files.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import multi_reg_file_steps
from cucumber_utils import *


@when(parsers.parse('{user} writes {megabytes} MB of random characters to {file} and saves MD5'))
def write_rand_text(user, megabytes, file, context):
    multi_reg_file_steps.write_rand_text(user, megabytes, file, "client1", context)


@when(parsers.parse('{user} writes "{text}" to {file}'))
def write_text(user, text, file, context):
    multi_reg_file_steps.write_text(user, text, file, "client1", context)


@when(parsers.parse('{user} reads "{text}" from {file}'))
@then(parsers.parse('{user} reads "{text}" from {file}'))
def read(user, text, file, context):
    multi_reg_file_steps.read(user, text, file, "client1", context)


@then(parsers.parse('{user} appends "{text}" to {file}'))
def append(user, text, file, context):
    multi_reg_file_steps.append(user, text, file, "client1", context)


@when(parsers.parse('{user} replaces "{text1}" with "{text2}" in {file}'))
def replace(user, text1, text2, file, context):
    multi_reg_file_steps.replace(user, text1, text2, file, "client1", context)


@when(parsers.parse('{user} executes {file}'))
@then(parsers.parse('{user} executes {file}'))
def execute_script(user, file, context):
    multi_reg_file_steps.execute_script(user, file, "client1", context)


@then(parsers.parse('{user} checks MD5 of {file}'))
def check_md5(user, file, context):
    multi_reg_file_steps.check_md5(user, file, "client1", context)


@when(parsers.parse('{user} copies regular file {file} to {path}'))
def copy_reg_file(user, file, path, context):
    multi_reg_file_steps.copy_reg_file(user, file, path, "client1", context)


@when(parsers.parse('{user} changes {file} size to {new_size} bytes'))
def do_truncate(user, file, new_size, context):
    multi_reg_file_steps.do_truncate(user, file, new_size, "client1", context)
