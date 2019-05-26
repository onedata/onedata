"""Module implements common steps for operation on files (both regular files
and directories)in multi-client environment.
"""

__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.utils.acceptance_utils import *
from tests.utils.utils import assert_generic, assert_
from tests.utils.client_utils import (ls, mv, chmod, stat, rm, touch,
                                      create_file, osrename, setxattr,
                                      getxattr, listxattr, removexattr)
from tests.utils.docker_utils import run_cmd

import os
import stat as stat_lib
import json, jsondiff
import pytest


@when(parsers.re('(?P<user>\w+) updates (?P<files>.*) timestamps on'
                 ' (?P<client_node>.*)'))
def touch_file(user, files, client_node, context):
    touch_file_base(user, files, client_node, context)


@when(parsers.re('(?P<user>\w+) fails to update (?P<files>.*) timestamps '
                 'on (?P<client_node>.*)'))
def touch_file_fail(user, files, client_node, context):
    touch_file_base(user, files, client_node, context, should_fail=True)


def touch_file_base(user, files, client_node, context, should_fail=False):
    user = context.get_user(user)
    client = user.get_client(client_node)
    files = list_parser(files)

    for file in files:
        file_path = client.absolute_path(file)

        def condition():
            try:
                touch(client, file_path)
            except OSError:
                return True if should_fail else False
            else:
                return False if should_fail else True

        assert_(client.perform, condition)


@when(parsers.re('(?P<user>\w+) creates regular files (?P<files>.*) '
                 'on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) creates regular files (?P<files>.*) '
                 'on (?P<client_node>.*)'))
def create_reg_file(user, files, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    files = list_parser(files)

    for file in files:
        file_path = client.absolute_path(file)

        def condition():
            create_file(client, file_path)

        assert_(client.perform, condition)


@when(parsers.re('(?P<user>\w+) creates children files of (?P<parent_dir>.*) '
                 'with names in range \[(?P<lower>.*), (?P<upper>.*)\) on '
                 '(?P<client_node>.*)'), converters=dict(lower=int, upper=int))
@then(parsers.re('(?P<user>\w+) creates children files of (?P<parent_dir>.*) '
                 'with names in range \[(?P<lower>.*), (?P<upper>.*)\) on '
                 '(?P<client_node>.*)'), converters=dict(lower=int, upper=int))
def create_many(user, lower, upper, parent_dir, client_node, context):
    for i in range(lower, upper):
        new_file = os.path.join(parent_dir, str(i))
        create_reg_file(user, make_arg_list(new_file), client_node, context)


@wt(parsers.re('(?P<user>\w+) can stat (?P<files>.*) in (?P<path>.*)'
               ' on (?P<client_node>.*)'))
def stat_present(user, path, files, client_node, context):
    client = context.get_client(user, client_node)
    path = client.absolute_path(path)
    files = list_parser(files)

    def condition():
        for f in files:
            stat(client, os.path.join(path, f))

    assert_(client.perform, condition)


@wt(parsers.re('(?P<user>\w+) can\'t stat (?P<files>.*) in (?P<path>.*) on '
               '(?P<client_node>.*)'))
def stat_absent(user, path, files, client_node, context):
    client = context.get_client(user, client_node)
    path = client.absolute_path(path)
    files = list_parser(files)

    def condition():
        for f in files:
            with pytest.raises(OSError, 
                               message = 'File {} exists in {}'.format(f, path)):
                stat(client, os.path.join(path, f))

    assert_(client.perform, condition)


@when(parsers.re('(?P<directory>.*) is empty for (?P<user>\w+) on (?P<client_node>.*)'))
@then(parsers.re('(?P<directory>.*) is empty for (?P<user>\w+) on (?P<client_node>.*)'))
def ls_empty(directory, user, client_node, context):
    client = context.get_client(user, client_node)
    dir_path = client.absolute_path(directory)

    def condition():
        assert len(ls(client, dir_path)) == 0

    assert_(client.perform, condition)


@when(parsers.re('(?P<user>\w+) sees (?P<files>.*) in (?P<path>.*) '
                 'on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) sees (?P<files>.*) in (?P<path>.*) '
                 'on (?P<client_node>.*)'))
def ls_present(user, files, path, client_node, context):
    client = context.get_client(user, client_node)
    path = client.absolute_path(path)
    files = list_parser(files)

    def condition():
        listed_files = ls(client, path)
        for file in files:
            assert file in listed_files

    assert_(client.perform, condition)


@when(parsers.re('(?P<user>\w+) lists only children of (?P<parent_dir>.*) with names'
                 ' in range \[(?P<lower>.*), (?P<upper>.*)\) on (?P<client_node>.*)'), 
                 converters=dict(lower=int,upper=int))
@then(parsers.re('(?P<user>\w+) lists only children of (?P<parent_dir>.*) with names'
                 ' in range \[(?P<lower>.*), (?P<upper>.*)\) on (?P<client_node>.*)'), 
                 converters=dict(lower=int,upper=int))
def ls_children(user, parent_dir, lower, upper, client_node, context):

    client = context.get_client(user, client_node)
    path = client.absolute_path(parent_dir)
    files_num = upper - lower

    def condition():
        listed_files = ls(client, path)
        assert len(listed_files) == files_num
        for i in range(lower, upper):
            assert str(i) in listed_files

    assert_(client.perform, condition)


@when(parsers.re('(?P<user>\w+) doesn\'t see (?P<files>.*) in (?P<path>.*) '
                 'on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) doesn\'t see (?P<files>.*) in (?P<path>.*) '
                 'on (?P<client_node>.*)'))
def ls_absent(user, files, path, client_node, context):
    client = context.get_client(user, client_node)
    path = client.absolute_path(path)
    files = list_parser(files)

    def condition():
        listed_files = ls(client, path)
        for file in files:
            assert file not in listed_files

    assert_(client.perform, condition)


@when(parsers.re('(?P<user>\w+) moves (?P<file1>.*) to (?P<file2>.*) '
                 'using shell command on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) moves (?P<file1>.*) to (?P<file2>.*) '
                 'using shell command on (?P<client_node>.*)'))
def shell_move(user, file1, file2, client_node, context):
    shell_move_base(user, file1, file2, client_node, context)


@when(parsers.re('(?P<user>\w+) fails to move (?P<file1>.*) to (?P<file2>.*) '
                 'using shell command on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) fails to move (?P<file1>.*) to (?P<file2>.*) '
                 'using shell command on (?P<client_node>.*)'))
def shell_move_fail(user, file1, file2, client_node, context):
    shell_move_base(user, file1, file2, client_node, context, should_fail=True)


def shell_move_base(user, file1, file2, client_node, context, should_fail=False):
    user = context.get_user(user)
    client = user.get_client(client_node)
    src = client.absolute_path(file1)
    dest = client.absolute_path(file2)

    def condition():
        mv(client, src, dest)
        cmd = "mv (?P<0>.*) (?P<1>.*)".format(src, dest)
        run_cmd(user.name, client, cmd, output=True, error=True)

    assert_generic(client.perform, should_fail, condition)


@when(parsers.re('(?P<user>\w+) renames (?P<file1>.*) to (?P<file2>.*)'
                 ' on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) renames (?P<file1>.*) to (?P<file2>.*)'
                 ' on (?P<client_node>.*)'))
def rename(user, file1, file2, client_node, context):
    mv_base(user, file1, file2, client_node, context)


@when(parsers.re('(?P<user>\w+) fails to rename (?P<file1>.*) to '
                 '(?P<file2>.*) on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) fails to rename (?P<file1>.*) to '
                 '(?P<file2>.*) on (?P<client_node>.*)'))
def rename_fail(user, file1, file2, client_node, context):
    rename_base(user, file1, file2, client_node, context, should_fail=True)


def rename_base(user, file1, file2, client_node, context, should_fail=False):
    user = context.get_user(user)
    client = user.get_client(client_node)
    src = client.absolute_path(file1)
    dest = client.absolute_path(file2)

    def condition():
        osrename(client, src, dest)

    assert_generic(client.perform, should_fail, condition)


def mv_base(user, file1, file2, client_node, context, should_fail=False):
    user = context.get_user(user)
    client = user.get_client(client_node)
    src = client.absolute_path(file1)
    dest = client.absolute_path(file2)

    def condition():
        mv(client, src, dest)

    assert_generic(client.perform, should_fail, condition)


@when(parsers.re('(?P<user>\w+) deletes files (?P<files>.*) on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) deletes files (?P<files>.*) on (?P<client_node>.*)'))
def delete_file(user, files, client_node, context):
    delete_file_base(user, files, client_node, context)


@when(parsers.re('(?P<user>\w+) fails to delete files (?P<files>.*) '
                 'on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) fails to delete files (?P<files>.*) '
                 'on (?P<client_node>.*)'))
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


@when(parsers.re('(?P<user>\w+) changes (?P<file>.*) mode to (?P<mode>.*) on '
                 '(?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) changes (?P<file>.*) mode to (?P<mode>.*) on '
                 '(?P<client_node>.*)'))
def change_mode(user, file, mode, client_node, context):
    change_mode_base(user, file, mode, client_node, context)


@when(parsers.re('(?P<user>\w+) fails to change (?P<file>.*) mode to '
                 '(?P<mode>.*) on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) fails to change (?P<file>.*) mode to '
                 '(?P<mode>.*) on (?P<client_node>.*)'))
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


@then(parsers.re('file type of (?P<user>\w+)\'s (?P<file>.*) is (?P<file_type>.*) '
                 'on (?P<client_node>.*)'))
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


@then(parsers.re('(?P<user>\w+) checks using shell stat if file type '
                 'of (?P<file>.*) is (?P<file_type>.*) on (?P<client_node>.*)'))
def shell_check_type(user, file, file_type, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    def condition():
        cmd = "stat --format=%F {}".format(file_path)
        stat_file_type = run_cmd(user.name, client, cmd, output=True)
        assert stat_file_type == file_type

    assert_(client.perform, condition)


@when(parsers.re('mode of (?P<user>\w+)\'s (?P<file>.*) is (?P<mode>.*) on '
                 '(?P<client_node>.*)'))
@then(parsers.re('mode of (?P<user>\w+)\'s (?P<file>.*) is (?P<mode>.*) on '
                 '(?P<client_node>.*)'))
def check_mode(user, file, mode, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)
    mode = int(mode, 8)

    def condition():
        stat_result = stat(client, file_path)
        assert stat_lib.S_IMODE(stat_result.st_mode) == mode

    assert_(client.perform, condition)


@when(parsers.re('size of (?P<user>\w+)\'s (?P<file>.*) is (?P<size>.*) bytes '
                 'on (?P<client_node>.*)'))
@then(parsers.re('size of (?P<user>\w+)\'s (?P<file>.*) is (?P<size>.*) bytes '
                 'on (?P<client_node>.*)'))
def check_size(user, file, size, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)
    size = int(size)

    def condition():
        stat_result = stat(client, file_path)
        assert stat_result.st_size == size

    assert_(client.perform, condition)


@when(parsers.re('(?P<user>\w+) records (?P<files>.*) '
                 'stats on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) records (?P<files>.*) '
                 'stats on (?P<client_node>.*)'))
def record_stats(user, files, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)

    for file_ in list_parser(files):
        file_path = client.absolute_path(file_)
        client.file_stats[file_path] = stat(client, file_path)


@then(parsers.re('(?P<time1>.*) time of (?P<user>\w+)\'s (?P<file>.*) is '
                 '(?P<comparator>.*) to (?P<time2>.*) time on (?P<client_node>.*)'))
@then(parsers.re('(?P<time1>.*) time of (?P<user>\w+)\'s (?P<file>.*) is '
                 '(?P<comparator>.*) than (?P<time2>.*) time on (?P<client_node>.*)'))
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


@then(parsers.re('(?P<time1>.*) time of (?P<user>\w+)\'s (?P<file1>.*) is '
                 '(?P<comparator>.*) to recorded one of (?P<file2>.*)'))
@then(parsers.re('(?P<time1>.*) time of (?P<user>\w+)\'s (?P<file1>.*) is '
                 '(?P<comparator>.*) than recorded one of (?P<file2>.*)'))
def cmp_time_to_previous(user, time1, comparator, file1, file2,
                         client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    attr = time_attr(time1)
    file_path = client.absolute_path(file1)
    recorded_stats = client.file_stats[client.absolute_path(file2)]

    def condition():
        stat_result = stat(client, file_path)
        t1 = getattr(stat_result, attr)
        t2 = getattr(recorded_stats, attr)
        assert compare(t1, t2, comparator)

    assert_(client.perform, condition)


@then(parsers.re('(?P<user>\w+) sets extended attribute (?P<name>[.\w]+) '
                 'with value (?P<value>.*) on (?P<file>\w+)'
                 'on (?P<client_node>.*)'))
@when(parsers.re('(?P<user>\w+) sets extended attribute (?P<name>[.\w]+) '
                 'with value (?P<value>.*) on (?P<file>\w+)'
                 'on (?P<client_node>.*)'))
def set_xattr(user, file, name, value, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    def condition():
        value_bytes = None
        if isinstance(value, str):
           value_bytes = value
        elif isinstance(value, unicode):
           value_bytes = value.encode('utf-8')
        else:
           value_bytes = str(value)
        
        setxattr(client, file_path, name, value_bytes)

    assert_(client.perform, condition)


@then(parsers.re('(?P<user>\w+) removes extended attribute (?P<name>[.\w]+) '
                 'from (?P<file>\w+) on (?P<client_node>.*)'))
@when(parsers.re('(?P<user>\w+) removes extended attribute (?P<name>[.\w]+) '
                 'from (?P<file>\w+) on (?P<client_node>.*)'))
def remove_xattr(user, file, name, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    def condition():
        removexattr(client, file_path, name)

    assert_(client.perform, condition)


@then(parsers.re('(?P<user>\w+) checks if (?P<file>\w+) has extended '
                 'attribute (?P<name>[.\w]+) on (?P<client_node>.*)'))
def check_xattr_exists(user, file, name, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    def condition():
        xattrs = listxattr(client, file_path)
        assert name in xattrs

    assert_(client.perform, condition)


@then(parsers.re('(?P<user>\w+) checks if (?P<file>\w+) does not have extended '
                 'attribute (?P<name>[.\w]+) on (?P<client_node>.*)'))
def check_xattr_doesnt_exist(user, file, name, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    def condition():
        xattrs = listxattr(client, file_path)
        assert name not in xattrs

    assert_(client.perform, condition)


@then(parsers.re('(?P<user>\w+) checks if (?P<file>\w+) has extended '
                 'attribute (?P<name>[.\w]+) with string value "(?P<value>.*)" '
                 'on (?P<client_node>.*)'))
def check_string_xattr(user, file, name, value, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    def condition():
        xattr_value = getxattr(client, file_path, name)
        value_utf = None
        if isinstance(value, str):
           value_utf = value
        elif isinstance(value, unicode):
           value_utf = value.encode('utf-8')
        else:
           value_utf = str(value)

        assert xattr_value == value_utf

    assert_(client.perform, condition)


@then(parsers.re('(?P<user>\w+) checks if (?P<file>\w+) has extended '
                 'attribute (?P<name>[.\w]+) with numeric value (?P<value>.*) '
                 'on (?P<client_node>.*)'))
def check_numeric_xattr(user, file, name, value, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    def condition():
        xattr_value = getxattr(client, file_path, name)
        assert float(xattr_value) == float(value)

    assert_(client.perform, condition)


@then(parsers.re('(?P<user>\w+) checks if (?P<file>\w+) has extended '
                 'attribute (?P<name>[.\w]+) with JSON value "(?P<value>.*)" '
                 'on (?P<client_node>.*)'))
def check_json_xattr(user, file, name, value, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    def condition():
        xattr_value = getxattr(client, file_path, name)
        assert jsondiff.diff(json.loads(xattr_value), json.loads(value)) == {}

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
