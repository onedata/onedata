"""Module implements common steps for operation on files (both regular files
and directories).
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import multi_file_steps
from cucumber_utils import *


@when(parsers.parse('{user} updates {files} timestamps'))
@when(parsers.parse('{user} creates regular files {files}'))
@then(parsers.parse('{user} creates regular files {files}'))
def create_reg_file(user, files, context):
    multi_file_steps.create_reg_file(user, files, "client1", context)


@when(parsers.parse('{user} sees {files} in {path}'))
@then(parsers.parse('{user} sees {files} in {path}'))
def ls_present(user, files, path, context):
    multi_file_steps.ls_present(user, files, path, "client1", context)


@when(parsers.parse('{user} doesn\'t see {files} in {path}'))
@then(parsers.parse('{user} doesn\'t see {files} in {path}'))
def ls_absent(user, files, path, context):
    multi_file_steps.ls_absent(user, files, path, "client1", context)


@when(parsers.parse('{user} renames {file1} to {file2}'))
def rename(user, file1, file2, context):
    multi_file_steps.rename(user, file1, file2, "client1", context)


@when(parsers.parse('{user} deletes files {files}'))
def delete_file(user, files, context):
    multi_file_steps.delete_file(user, files, "client1", context)


@then(parsers.parse('file type of {user}\'s {file} is {fileType}'))
def check_type(user, file, fileType, context):
    multi_file_steps.check_type(user, file, fileType, "client1", context)


@then(parsers.parse('mode of {user}\'s {file} is {mode}'))
def check_mode(user, file, mode, context):
    multi_file_steps.change_mode(user, file, mode, "client1", context)


@then(parsers.parse('{user} changes {file} mode to {mode}'))
@when(parsers.parse('{user} changes {file} mode to {mode}'))
def change_mode(user, file, mode, context):
    multi_file_steps.change_mode(user, file, mode, "client1", context)


@when(parsers.parse('size of {user}\'s {file} is {size} bytes'))
@then(parsers.parse('size of {user}\'s {file} is {size} bytes'))
def check_size(user, file, size, context):
    multi_file_steps.check_size(user, file, size, "client1", context)


@then(parsers.parse('{time1} time of {user}\'s {file} is {comparator} than {time2} time'))
@then(parsers.parse('{time1} time of {user}\'s {file} is {comparator} to {time2} time'))
def check_time(user, time1, time2, comparator, file, context):
    multi_file_steps.check_time(user, time1, time2, comparator, file, "client1",
                                context)
