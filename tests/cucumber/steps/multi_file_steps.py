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
                                      get_client)


@when(parsers.parse('{user} updates {files} timestamps on {client_node}'))
@when(parsers.parse('{user} creates regular files {files} on {client_node}'))
def create_reg_file(user, files, client_node, context):
    client = get_client(client_node, user, context)
    files = list_parser(files)
    for file in files:
        file_path = client_mount_path(file, client)

        def condition():
            return_code = touch(client, file_path, user)
            save_op_code(context, user, return_code)
            return return_code == 0

        assert repeat_until(condition, client.timeout)


@when(parsers.parse('{user} sees {files} in {path} on {client_node}'))
@then(parsers.parse('{user} sees {files} in {path} on {client_node}'))
def ls_present(user, files, path, client_node, context):
    client = get_client(client_node, user, context)
    path = client_mount_path(path, client)
    files = list_parser(files)

    def condition():

        try:
            cmd_output = ls(client, user, path).split()
            print "ls: ", cmd_output
            for file in files:
                if file not in cmd_output:
                    return False
            return True
        except subprocess.CalledProcessError:
            return False

    assert repeat_until(condition, client.timeout)


@when(parsers.parse('{user} doesn\'t see {files} in {path} on {client_node}'))
@then(parsers.parse('{user} doesn\'t see {files} in {path} on {client_node}'))
def ls_absent(user, files, path, client_node, context):
    client = get_client(client_node, user, context)
    path = client_mount_path(path, client)
    files = list_parser(files)

    def condition():
        try:
            cmd_output = ls(client, user, path).split()
            print "ls: ", cmd_output
            for file in files:
                if file in cmd_output:
                    return False
            return True
        except subprocess.CalledProcessError:
            return False

    assert repeat_until(condition, client.timeout)


@when(parsers.parse('{user} renames {file1} to {file2} on {client_node}'))
def rename(user, file1, file2, client_node, context):
    client = get_client(client_node, user, context)
    src = client_mount_path(file1, client)
    dest = client_mount_path(file2, client)

    def condition():
        cmd_return_code = mv(client, src, dest, user)
        save_op_code(context, user, cmd_return_code)
        return cmd_return_code == 0

    repeat_until(condition, client.timeout)


@when(parsers.parse('{user} deletes files {files} on {client_node}'))
def delete_file(user, files, client_node, context):
    client = get_client(client_node, user, context)
    files = list_parser(files)
    for file in files:
        path = client_mount_path(file, client)

        def condition():
            ret = rm(client, path, user=user)
            save_op_code(context, user, ret)
            return ret == 0

        repeat_until(condition, timeout=client.timeout)


@when(parsers.parse('{user} changes {file} mode to {mode} on {client_node}'))
@then(parsers.parse('{user} changes {file} mode to {mode} on {client_node}'))
def change_mode(user, file, mode, client_node, context):
    client = get_client(client_node, user, context)
    mode = str(mode)
    file_path = client_mount_path(file, client)

    def condition():
        cmd_return_code = chmod(client, mode, file_path, user)
        save_op_code(context, user, cmd_return_code)
        return cmd_return_code == 0

    repeat_until(condition, client.timeout)


@then(parsers.parse('file type of {user}\'s {file} is {file_type} on {client_node}'))
def check_type(user, file, file_type, client_node, context):
    client = get_client(client_node, user, context)
    file_path = client_mount_path(file, client)
    check_using_stat(user, client, file_path, 'file type', file_type)


@when(parsers.parse('mode of {user}\'s {file} is {mode} on {client_node}'))
@then(parsers.parse('mode of {user}\'s {file} is {mode} on {client_node}'))
def check_mode(user, file, mode, client_node, context):
    client = get_client(client_node, user, context)
    mode = str(mode)
    file_path = client_mount_path(file, client)
    check_using_stat(user, client, file_path, 'mode', mode)


@when(parsers.parse('size of {user}\'s {file} is {size} bytes on {client_node}'))
@then(parsers.parse('size of {user}\'s {file} is {size} bytes on {client_node}'))
def check_size(user, file, size, client_node, context):
    client = get_client(client_node, user, context)
    file_path = client_mount_path(file, client)
    size = str(size)
    check_using_stat(user, client, file_path, 'size', size)


@then(parsers.parse('{time1} time of {user}\'s {file} is {comparator} to {time2} time on {client_node}'))
@then(parsers.parse('{time1} time of {user}\'s {file} is {comparator} than {time2} time on {client_node}'))
def check_time(user, time1, time2, comparator, file, client_node, context):
    client = get_client(client_node, user, context)
    opt1 = get_stat_option(time1)
    opt2 = get_stat_option(time2)
    file_path = client_mount_path(file, client)

    def condition():

        try:
            times = stat(client, file_path, user=user,
                         format="{t1} {t2}".format(t1=opt1, t2=opt2))
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
            cmd_output = stat(client, file_path, format=opt, user=user)
            return cmd_output == expected_value
        except subprocess.CalledProcessError:
            return False

    assert repeat_until(condition, client.timeout)


def get_timestamp(user, file, client, time_type):
    opt = get_stat_option(time_type)
    file_path = client_mount_path(file, client)
    return stat(client, file_path, format=opt, user=user)


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
