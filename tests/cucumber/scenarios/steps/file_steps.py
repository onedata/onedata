"""Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements common steps for operation on files (both regular files
and directories).
"""

import pytest
from pytest_bdd import (given, when, then)
from pytest_bdd import parsers

from environment import docker, env
from common import *


@when(parsers.parse('{user} updates {files} timestamps'))
@when(parsers.parse('{user} creates regular files {files}'))
def create_reg_file(user, files, client_id, context):
    files = list_parser(files)
    for file in files:
        ret = docker.exec_(container=client_id,
                     command=["touch", make_path(context, file)])
        save_op_code(context, ret)


@then(parsers.parse('{files} are in ls {path}'))
def ls_present(files, path, client_id, context):
    cmd = ["ls", make_path(context, path)]
    ls_files = docker.exec_(container=client_id, command=cmd, output=True).split()
    files = list_parser(files)
    for file in files:
        assert file in ls_files


@then(parsers.parse('{files} are not in ls {path}'))
def ls_absent(files, path, client_id, context):
    cmd = ["ls", make_path(context, path)]
    ls_files = docker.exec_(container=client_id, command=cmd, output=True).split()
    files = list_parser(files)
    for file in files:
        assert file not in ls_files


@when(parsers.parse('{user} renames {file1} to {file2}'))
def rename(user, file1, file2, client_id, context):
    ret = docker.exec_(container=client_id,
                       command=["mv", make_path(context, file1), make_path(context, file2)])
    save_op_code(context, ret)


@when(parsers.parse('{user} deletes files {files}'))
def delete_file(user, files, client_id, context):
    files = list_parser(files)
    for file in files:
        ret = docker.exec_(container=client_id,
                           command=["rm", make_path(context, file)])
        save_op_code(context, ret)


@then(parsers.parse('{file} file type is {fileType}'))
def check_type(file, fileType, client_id, context):
    currFileType = docker.exec_(container=client_id,
                                command=["stat", make_path(context, file), "--format=%F"],
                                output=True)
    assert fileType == currFileType


@then(parsers.parse('{file} mode is {mode}'))
def check_mode(file, mode, client_id, context):
    curr_mode = docker.exec_(container=client_id,
                             command=["stat", "--format=%a", make_path(context, file)],
                             output=True)
    assert mode == curr_mode


@when(parsers.parse('{user} changes {file} mode to {mode}'))
def change_mode(user, file, mode, client_id, context):
    docker.exec_(container=client_id,
                 command=["chmod", mode, make_path(context, file)])


@then("clean succeeds")
def clean(client_id, context):
    spaces = docker.exec_(container=client_id, command=['ls', make_path(context, 'spaces')],
                          output=True)
    spaces = spaces.split("\n")

    # clean spaces
    for space in spaces:
        docker.exec_(container=client_id,
                     command="rm -rf " + make_path(context, 'spaces/' + space + '/*'))

    pid = docker.exec_(container=client_id,
                       command="ps aux | grep './oneclient --authentication token' | " +
                       "grep -v 'grep' | awk '{print $2}'", output=True)

    # kill oneclient process
    docker.exec_(container=client_id, command="kill -KILL " + str(pid), output=True)

    # unmount onedata
    docker.exec_(container=client_id, command="umount " + context.mount_path)

    # remove onedata dir
    ret = docker.exec_(container=client_id, command="rm -rf " + context.mount_path)

    save_op_code(context, ret)
    success(context)


@then(parsers.parse('{file} size is {size} bytes'))
def check_size(file, size, context, client_id):
    curr_size = docker.exec_(container=client_id,
                             command=["stat", "--format=%s", make_path(context, file)],
                             output=True)
    assert size == curr_size


@then(parsers.parse('{time1} of {file} is {comparator} to {time2}'))
@then(parsers.parse('{time1} of {file} is {comparator} than {time2}'))
def check_time(time1, time2, comparator, file, context, client_id):

    opt1 = get_time_opt(time1)
    opt2 = get_time_opt(time2)
    file = str(file)

    time1 = docker.exec_(container=client_id,
                         command="stat --format=%" + opt1 + ' ' + make_path(context, file),
                         output=True)
    time2 = docker.exec_(container=client_id,
                         command="stat --format=%" + opt2 + ' ' + make_path(context, file),
                         output=True)

    assert compare(int(time1), int(time2), comparator)


####################################################################################################


def get_time_opt(time):
    if time == "access time":
        return 'X'
    elif time == "modification time":
        return 'Y'
    elif time == "status-change time":
        return 'Z'
    else:
        raise ValueError("Wrong argument to function get_time_opt")


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
