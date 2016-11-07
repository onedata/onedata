"""Module implements common steps for operation on files (both regular files
and directories)in multi-client environment.
"""

__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.utils.acceptance_utils import *
from tests.utils.utils import assert_generic, assert_
from tests.utils.client_utils import ls, mv, chmod, stat, rm, touch, create_file
from tests.utils.docker_utils import run_cmd

import os
import stat as stat_lib


@when(parsers.parse('{user} updates {files} timestamps on {client_node}'))
def touch_file(user, files, client_node, context):
    touch_file_base(user, files, client_node, context)


@when(parsers.parse('{user} fails to update {files} timestamps on {client_node}'))
def touch_file_fail(user, files, client_node, context):
    touch_file_base(user, files, client_node, context, should_fail=True)


def touch_file_base(user, files, client_node, context, should_fail=False):
    user = context.get_user(user)
    client = user.get_client(client_node)
    files = list_parser(files)

    for file in files:
        file_path = client.absolute_path(file)

        def condition():
            touch(client, file_path)

        assert_generic(client.perform, should_fail, condition)


@when(parsers.parse('{user} creates regular files {files} on {client_node}'))
@then(parsers.parse('{user} creates regular files {files} on {client_node}'))
def create_reg_file(user, files, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    files = list_parser(files)

    for file in files:
        file_path = client.absolute_path(file)

        def condition():
            create_file(client, file_path)

        assert_(client.perform, condition)


@when(parsers.parse('{user} creates children files of {parent_dir} with names '
                    'in range [{lower:d}, {upper:d}) on {client_node}'))
@then(parsers.parse('{user} creates children files of {parent_dir} with names '
                    'in range [{lower:d}, {upper:d}) on {client_node}'))
def create_many(user, lower, upper, parent_dir, client_node, context):
    for i in range(lower, upper):
        new_file = os.path.join(parent_dir, str(i))
        create_reg_file(user, make_arg_list(new_file), client_node, context)


@when(parsers.parse('{user} sees {files} in {path} on {client_node}'))
@then(parsers.parse('{user} sees {files} in {path} on {client_node}'))
def ls_present(user, files, path, client_node, context):
    client = context.get_client(user, client_node)
    path = client.absolute_path(path)
    files = list_parser(files)

    def condition():
        listed_files = ls(client, path)
        for file in files:
            assert file in listed_files

    assert_(client.perform, condition)


@when(parsers.parse('{user} lists only children of {parent_dir} with names in '
                    'range [{lower:d}, {upper:d}) on {client_node}'))
@then(parsers.parse('{user} lists only children of {parent_dir} with names in '
                    'range [{lower:d}, {upper:d}) on {client_node}'))
def ls_children(user, parent_dir, lower, upper, client_node, context):

    client = context.get_client(user, client_node)
    path = client.absolute_path(parent_dir)
    files_num = upper - lower

    def condition():
        listed_files = ls(client, path)
        assert len(listed_files) == files_num
        for i in range(lower, upper):
            assert str(i) in listed_files

    assert client.perform(condition)


@when(parsers.parse('{user} doesn\'t see {files} in {path} on {client_node}'))
@then(parsers.parse('{user} doesn\'t see {files} in {path} on {client_node}'))
def ls_absent(user, files, path, client_node, context):
    client = context.get_client(user, client_node)
    path = client.absolute_path(path)
    files = list_parser(files)

    def condition():
        listed_files = ls(client, path)
        for file in files:
            assert file not in listed_files

    assert_(client.perform, condition)


@when(parsers.parse('{user} moves {file1} to {file2} using shell command on {client_node}'))
@then(parsers.parse('{user} moves {file1} to {file2} using shell command on {client_node}'))
def shell_move(user, file1, file2, client_node, context):
    shell_move_base(user, file1, file2, client_node, context)


@when(parsers.parse('{user} fails to move {file1} to {file2} using shell command on {client_node}'))
@then(parsers.parse('{user} fails to move {file1} to {file2} using shell command on {client_node}'))
def shell_move_fail(user, file1, file2, client_node, context):
    shell_move_base(user, file1, file2, client_node, context, should_fail=True)


def shell_move_base(user, file1, file2, client_node, context, should_fail=False):
    user = context.get_user(user)
    client = user.get_client(client_node)
    src = client.absolute_path(file1)
    dest = client.absolute_path(file2)

    def condition():
        mv(client, src, dest)
        cmd = "mv {0} {1}".format(src, dest)
        run_cmd(user.name, client, cmd, output=True, error=True)

    assert_generic(client.perform, should_fail, condition)


@when(parsers.parse('{user} renames {file1} to {file2} on {client_node}'))
@then(parsers.parse('{user} renames {file1} to {file2} on {client_node}'))
def rename(user, file1, file2, client_node, context):
    rename_base(user, file1, file2, client_node, context)


@when(parsers.parse('{user} fails to rename {file1} to {file2} on {client_node}'))
@then(parsers.parse('{user} fails to rename {file1} to {file2} on {client_node}'))
def rename_fail(user, file1, file2, client_node, context):
    rename_base(user, file1, file2, client_node, context, should_fail=True)


def rename_base(user, file1, file2, client_node, context, should_fail=False):
    user = context.get_user(user)
    client = user.get_client(client_node)
    src = client.absolute_path(file1)
    dest = client.absolute_path(file2)

    def condition():
        mv(client, src, dest)

    assert_generic(client.perform, should_fail, condition)


@when(parsers.parse('{user} deletes files {files} on {client_node}'))
@then(parsers.parse('{user} deletes files {files} on {client_node}'))
def delete_file(user, files, client_node, context):
    delete_file_base(user, files, client_node, context)


@when(parsers.parse('{user} fails to delete files {files} on {client_node}'))
@then(parsers.parse('{user} fails to delete files {files} on {client_node}'))
def delete_file_fail(user, files, client_node, context):
    delete_file_base(user, files, client_node, context, should_fail=True)


def delete_file_base(user, files, client_node, context, should_fail=False):
    user = context.get_user(user)
    client = user.get_client(client_node)
    files = list_parser(files)
    for file in files:
        path = client.absolute_path(file)

        def condition():
            rm(client, path)

        assert_generic(client.perform, should_fail, condition)


@when(parsers.parse('{user} changes {file} mode to {mode} on {client_node}'))
@then(parsers.parse('{user} changes {file} mode to {mode} on {client_node}'))
def change_mode(user, file, mode, client_node, context):
    change_mode_base(user, file, mode, client_node, context)


@when(parsers.parse('{user} fails to change {file} mode to {mode} on {client_node}'))
@then(parsers.parse('{user} fails to change {file} mode to {mode} on {client_node}'))
def change_mode_fail(user, file, mode, client_node, context):
    change_mode_base(user, file, mode, client_node, context, should_fail=True)


def change_mode_base(user, file, mode, client_node, context, should_fail=False):
    user = context.get_user(user)
    client = user.get_client(client_node)
    mode = int(mode, 8)
    file_path = client.absolute_path(file)

    def condition():
        chmod(client, mode, file_path)

    assert_generic(client.perform, should_fail, condition)


@then(parsers.parse('file type of {user}\'s {file} is {file_type} on {client_node}'))
def check_type(user, file, file_type, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    if file_type == "regular":
        stat_method = "S_ISREG"
    elif file_type == "directory":
        stat_method = "S_ISDIR"

    def condition():
        stat_result = stat(client, file_path)
        assert getattr(stat_lib, stat_method)(stat_result.st_mode)

    assert_(client.perform, condition)


@then(parsers.parse('{user} checks using shell stat if file type of {file} is '
                    '{file_type} on {client_node}'))
def shell_check_type(user, file, file_type, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    def condition():
        cmd = "stat --format=%F {}".format(file_path)
        stat_file_type = run_cmd(user.name, client, cmd, output=True)
        assert stat_file_type == file_type

    assert_(client.perform, condition)


@when(parsers.parse('mode of {user}\'s {file} is {mode} on {client_node}'))
@then(parsers.parse('mode of {user}\'s {file} is {mode} on {client_node}'))
def check_mode(user, file, mode, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)
    mode = int(mode, 8)

    def condition():
        stat_result = stat(client, file_path)
        assert stat_lib.S_IMODE(stat_result.st_mode) == mode

    assert_(client.perform, condition)


@when(parsers.parse('size of {user}\'s {file} is {size} bytes on {client_node}'))
@then(parsers.parse('size of {user}\'s {file} is {size} bytes on {client_node}'))
def check_size(user, file, size, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)
    size = int(size)

    def condition():
        stat_result = stat(client, file_path)
        assert stat_result.st_size == size

    assert_(client.perform, condition)


@then(parsers.parse('{time1} time of {user}\'s {file} is {comparator} to {time2} time on {client_node}'))
@then(parsers.parse('{time1} time of {user}\'s {file} is {comparator} than {time2} time on {client_node}'))
def check_time(user, time1, time2, comparator, file, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    attr1 = time_attr(time1)
    attr2 = time_attr(time2)
    file_path = client.absolute_path(file)

    def condition():
        stat_result = stat(client, file_path)
        t1 = getattr(stat_result, attr1)
        t2 = getattr(stat_result, attr2)
        assert compare(t1, t2, comparator)

    assert_(client.perform, condition)
    

################################################################################

def time_attr(parameter):

    return{
        'access': 'st_atime',
        'modification': 'st_mtime',
        'status-change': 'st_ctime'
    }[parameter]


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
