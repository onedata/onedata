"""Module implements common steps for operation on files (both regular files
and directories)in multi-client environment.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import subprocess

from cucumber_utils import *
from tests.utils.client_utils import (ls, mv, chmod, stat, rm, touch,
                                      client_mount_path, save_op_code,
                                      get_client, create_file)


@when(parsers.parse('{user} updates {files} timestamps on {client_node}'))
# @when(parsers.parse('{user} creates regular files {files} on {client_node}'))
# @then(parsers.parse('{user} creates regular files {files} on {client_node}'))
def touch_file(user, files, client_node, context):
    client = context.get_client(user, client_node)
    files = list_parser(files)
    for file in files:
        file_path = client.absolute_path(file)

        def condition():
            return_code = touch(client, file_path, user)
            save_op_code(context, user, return_code)
            return return_code == 0

        # assert repeat_until(condition, client.timeout)
        assert client.perform(condition)


@when(parsers.parse('{user} creates regular files {files} on {client_node}'))
@then(parsers.parse('{user} creates regular files {files} on {client_node}'))
def create_reg_file(user, files, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    files = list_parser(files)
    for file in files:
        file_path = client.absolute_path(file)

        def condition():

            try:
                create_file(client, file_path)
                user.mark_last_operation_succeeded()
                return True
            except:
                user.mark_last_operation_failed()
                return False

        assert client.perform(condition)


@when(parsers.parse('{user} sees {files} in {path} on {client_node}'))
@then(parsers.parse('{user} sees {files} in {path} on {client_node}'))
def ls_present(user, files, path, client_node, context):
    client = context.get_client(user, client_node)
    path = client.absolute_path(path)
    files = list_parser(files)

    def condition():

        try:
            listed_files = ls(client, path)
            for file in files:
                if file not in listed_files:
                    return False
            return True
        except:
            return False

    assert client.perform(condition)


@when(parsers.parse('{user} doesn\'t see {files} in {path} on {client_node}'))
@then(parsers.parse('{user} doesn\'t see {files} in {path} on {client_node}'))
def ls_absent(user, files, path, client_node, context):
    client = context.get_client(user, client_node)
    path = client.absolute_path(path)
    files = list_parser(files)

    def condition():
        try:
            listed_files = ls(client, path)
            for file in files:
                if file in listed_files:
                    return False
            return True
        except:
            return False

    assert client.perform(condition)


@when(parsers.parse('{user} renames {file1} to {file2} on {client_node}'))
def rename(user, file1, file2, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    src = client.absolute_path(file1)
    dest = client.absolute_path(file2)

    def condition():

        try:
            mv(client, src, dest)
            user.mark_last_operation_succeeded()
            return True
        except:
            user.mark_last_operation_failed()
            return False

    client.perform(condition)


@when(parsers.parse('{user} deletes files {files} on {client_node}'))
@then(parsers.parse('{user} deletes files {files} on {client_node}'))
def delete_file(user, files, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    files = list_parser(files)
    for file in files:
        path = client.absolute_path(file)

        def condition():
            try:
                rm(client, path)
                user.mark_last_operation_succeeded()
                return True
            except:
                user.mark_last_operation_failed()
                return False

        client.perform(condition)


@when(parsers.parse('{user} changes {file} mode to {mode} on {client_node}'))
@then(parsers.parse('{user} changes {file} mode to {mode} on {client_node}'))
def change_mode(user, file, mode, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    mode = str(mode)
    file_path = client.absolute_path(file)

    def condition():

        try:
            chmod(client, mode, file_path)
            user.mark_last_operation_succeeded()
            return True
        except:
            user.mark_last_operation_failed()
            return False

    assert client.perform(condition)


@then(parsers.parse('file type of {user}\'s {file} is {file_type} on {client_node}'))
def check_type(user, file, file_type, client_node, context):
    client = context.get_client(user, client_node)
    file_path = client.absolute_path(file)
    check_using_stat(user, client, file_path, 'file type', file_type)


@when(parsers.parse('mode of {user}\'s {file} is {mode} on {client_node}'))
@then(parsers.parse('mode of {user}\'s {file} is {mode} on {client_node}'))
def check_mode(user, file, mode, client_node, context):
    client = context.get_client(user, client_node)
    mode = str(mode)
    file_path = client.absolute_path(file)
    check_using_stat(user, client, file_path, 'mode', mode)


@when(parsers.parse('size of {user}\'s {file} is {size} bytes on {client_node}'))
@then(parsers.parse('size of {user}\'s {file} is {size} bytes on {client_node}'))
def check_size(user, file, size, client_node, context):
    client = context.get_client(user, client_node)
    file_path = client.absolute_path(file)
    size = str(size)
    check_using_stat(user, client, file_path, 'size', size)


@then(parsers.parse('{time1} time of {user}\'s {file} is {comparator} to {time2} time on {client_node}'))
@then(parsers.parse('{time1} time of {user}\'s {file} is {comparator} than {time2} time on {client_node}'))
def check_time(user, time1, time2, comparator, file, client_node, context):
    client = context.get_client(user, client_node)
    opt1 = get_stat_option(time1)
    opt2 = get_stat_option(time2)
    file_path = client.absolute_path(file)

    def condition():

        try:
            times = stat(client, file_path)
            times = times.split(" ")
            return compare(int(times[0]), int(times[1]), comparator)
        except subprocess.CalledProcessError:
            return False

    assert repeat_until(condition, client.timeout)
    

################################################################################


def check_using_stat(user, client, file_path, parameter, expected_value):

    opt = get_stat_option(parameter)

    def condition():
        try:
            cmd_output = stat(client, file_path)
            return cmd_output == expected_value
        except subprocess.CalledProcessError:
            return False

    assert repeat_until(condition, client.timeout)


def get_timestamp(user, file, client, time_type):
    opt = get_stat_option(time_type)
    file_path = client.absolute_path(file)
    return stat(client, file_path)


def get_stat_option(parameter):

    formats = {
        'access': '%X',
        'modification': '%Y',
        'status-change': '%Z',
        'file type': '%F',
        'mode': '%a',
        'size': '%s'
    }

    return formats[parameter]


def compare(val1, val2, comparator):
    if comparator == 'equal':
        return val1 == val2
    elif comparator == 'not equal':
        return val1 != val2
    elif comparator == 'greater':
        return val1 > val2
    elif comparator == 'less':
        return val1 < val2
    elif comparator == 'not greater':
        return val1 <= val2
    elif comparator == 'not less':
        return val1 >= val2
    else:
        raise ValueError("Wrong argument comparator to function compare")
