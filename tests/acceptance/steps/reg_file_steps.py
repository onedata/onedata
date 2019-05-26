"""Module implements pytest-bdd steps for operations on regular files.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import multi_reg_file_steps
from tests.utils.acceptance_utils import *
from pytest_bdd import when, then, parsers


@when(parsers.re('(?P<user>\w+) writes (?P<megabytes>.*) MB of random characters to (?P<file>.*) and saves MD5'))
def write_rand_text(user, megabytes, file, context):
    multi_reg_file_steps.write_rand_text(user, megabytes, file, "client1", context)


@when(parsers.re('(?P<user>\w+) writes "(?P<text>.*)" to (?P<file>.*)'))
@then(parsers.re('(?P<user>\w+) writes "(?P<text>.*)" to (?P<file>.*)'))
def write_text(user, text, file, context):
    multi_reg_file_steps.write_text(user, text, file, "client1", context)


@when(parsers.re('(?P<user>\w+) fails to write "(?P<text>.*)" to (?P<file>.*)'))
@then(parsers.re('(?P<user>\w+) fails to write "(?P<text>.*)" to (?P<file>.*)'))
def write_text_fail(user, text, file, context):
    multi_reg_file_steps.write_text_fail(user, text, file, "client1", context)


@when(parsers.re('(?P<user>\w+) reads "(?P<text>.*)" from file (?P<file>.*)'))
@then(parsers.re('(?P<user>\w+) reads "(?P<text>.*)" from file (?P<file>.*)'))
def read(user, text, file, context):
    multi_reg_file_steps.read_text(user, text, file, "client1", context)


@when(parsers.re('(?P<user>\w+) appends "(?P<text>.*)" to (?P<file>.*)'))
@then(parsers.re('(?P<user>\w+) appends "(?P<text>.*)" to (?P<file>.*)'))
def append(user, text, file, context):
    multi_reg_file_steps.append(user, text, file, "client1", context)


@when(parsers.re('(?P<user>\w+) replaces "(?P<text1>.*)" with "(?P<text2>.*)" in (?P<file>.*)'))
def replace(user, text1, text2, file, context):
    multi_reg_file_steps.replace(user, text1, text2, file, "client1", context)


@when(parsers.re('(?P<user>\w+) executes (?P<file>.*)'))
@then(parsers.re('(?P<user>\w+) executes (?P<file>.*)'))
def execute_script(user, file, context):
    multi_reg_file_steps.execute_script(user, file, "client1", context)


@when(parsers.re('(?P<user>\w+) fails to execute (?P<file>.*)'))
@then(parsers.re('(?P<user>\w+) fails to execute (?P<file>.*)'))
def execute_script_fail(user, file, context):
    multi_reg_file_steps.execute_script_fail(user, file, "client1", context)


@then(parsers.re('(?P<user>\w+) checks MD5 of (?P<file>.*)'))
def check_md5(user, file, context):
    multi_reg_file_steps.check_md5(user, file, "client1", context)


@when(parsers.re('(?P<user>\w+) copies regular file (?P<file>.*) to (?P<path>.*)'))
def copy_reg_file(user, file, path, context):
    multi_reg_file_steps.copy_reg_file(user, file, path, "client1", context)


@when(parsers.re('(?P<user>\w+) changes (?P<file>.*) size to (?P<new_size>.*) bytes'))
def do_truncate(user, file, new_size, context):
    multi_reg_file_steps.do_truncate(user, file, new_size, "client1", context)


@when(parsers.re('(?P<user>\w+) fails to change (?P<file>.*) size to (?P<new_size>.*) bytes'))
@then(parsers.re('(?P<user>\w+) fails to change (?P<file>.*) size to (?P<new_size>.*) bytes'))
def do_truncate_fail(user, file, new_size, context):
    multi_reg_file_steps.do_truncate_fail(user, file, new_size, "client1", context)



@when(parsers.re('(?P<user>\w+) performs command "(?P<command>.*)" in (?P<path>.*) directory'))
@then(parsers.re('(?P<user>\w+) performs command "(?P<command>.*)" in (?P<path>.*) directory'))
def run_cmd_in_directory(user, command, path, context):
    multi_reg_file_steps.run_cmd_in_directory(user, command, path, "client1", context)
