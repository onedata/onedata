"""Module implements common steps for operation on files (both regular files
and directories).
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import multi_file_steps
from tests.utils.acceptance_utils import *


@when(parsers.re('(?P<user>\w+) updates (?P<files>.*) timestamps'))
def touch_file(user, files, context):
    multi_file_steps.touch_file(user, files, "client1", context)


@when(parsers.re('(?P<user>\w+) fails to update (?P<files>.*) timestamps'))
def touch_file_fail(user, files, context):
    multi_file_steps.touch_file_fail(user, files, "client1", context)


@when(parsers.re('(?P<user>\w+) creates regular files (?P<files>.*)'))
@then(parsers.re('(?P<user>\w+) creates regular files (?P<files>.*)'))
def create_reg_file(user, files, context):
    multi_file_steps.create_reg_file(user, files, "client1", context)


@when(parsers.re('(?P<user>\w+) creates children files of (?P<parent_dir>.*) '
                 'with names in range \[(?P<lower>.*), (?P<upper>.*)\)'), 
                 converters=dict(lower=int, upper=int))
@then(parsers.re('(?P<user>\w+) creates children files of (?P<parent_dir>.*) '
                 'with names in range \[(?P<lower>.*), (?P<upper>.*)\)'), 
                 converters=dict(lower=int, upper=int))
def create_many(user, lower, upper, parent_dir, context):
    multi_file_steps.create_many(user, lower, upper, parent_dir, "client1",
                                 context)


@wt(parsers.re('(?P<user>\w+) can stat (?P<files>.*) in (?P<path>.*)'))
def stat_present(user, path, files, context):
    multi_file_steps.stat_present(user, path, files, "client1", context)


@wt(parsers.re('(?P<user>\w+) can\'t stat (?P<files>.*) in (?P<path>.*)'))
def stat_absent(user, path, files, context):
    multi_file_steps.stat_absent(user, path, files, "client1", context)


@when(parsers.re('(?P<user>\w+) sees (?P<files>.*) in (?P<path>.*)'))
@then(parsers.re('(?P<user>\w+) sees (?P<files>.*) in (?P<path>.*)'))
def ls_present(user, files, path, context):
    multi_file_steps.ls_present(user, files, path, "client1", context)


@when(parsers.re('(?P<user>\w+) lists only children of (?P<parent_dir>.*) '
                 'with names in range \[(?P<lower>.*), (?P<upper>.*)\)'), 
                 converters=dict(lower=int, upper=int))
@then(parsers.re('(?P<user>\w+) lists only children of (?P<parent_dir>.*) '
                 'with names in range \[(?P<lower>.*), (?P<upper>.*)\)'), 
                 converters=dict(lower=int, upper=int))
def ls_children(user, parent_dir, lower, upper, context):
    multi_file_steps.ls_children(user, parent_dir, lower, upper, "client1",
                                 context)


@when(parsers.re('(?P<user>\w+) doesn\'t see (?P<files>.*) in (?P<path>.*)'))
@then(parsers.re('(?P<user>\w+) doesn\'t see (?P<files>.*) in (?P<path>.*)'))
def ls_absent(user, files, path, context):
    multi_file_steps.ls_absent(user, files, path, "client1", context)


@when(parsers.re('(?P<user>\w+) moves (?P<file1>.*) to (?P<file2>.*) using shell'
                 ' command'))
@then(parsers.re('(?P<user>\w+) moves (?P<file1>.*) to (?P<file2>.*) using shell'
                 ' command'))
def shell_move(user, file1, file2, context):
    multi_file_steps.shell_move(user, file1, file2, "client1", context)


@when(parsers.re('(?P<user>\w+) fails to move (?P<file1>.*) to (?P<file2>.*)'
                 ' using shell command'))
@then(parsers.re('(?P<user>\w+) fails to move (?P<file1>.*) to (?P<file2>.*)'
                 ' using shell command'))
def shell_move_fail(user, file1, file2, context):
    multi_file_steps.shell_move_fail(user, file1, file2, "client1", context)


@when(parsers.re('(?P<user>\w+) renames (?P<file1>.*) to (?P<file2>.*)'))
@then(parsers.re('(?P<user>\w+) renames (?P<file1>.*) to (?P<file2>.*)'))
def rename(user, file1, file2, context):
    multi_file_steps.rename(user, file1, file2, "client1", context)


@when(parsers.re('(?P<user>\w+) fails to rename (?P<file1>.*) to (?P<file2>.*)'))
@then(parsers.re('(?P<user>\w+) fails to rename (?P<file1>.*) to (?P<file2>.*)'))
def rename_fail(user, file1, file2, context):
    multi_file_steps.rename_fail(user, file1, file2, "client1", context)


@when(parsers.re('(?P<user>\w+) deletes files (?P<files>.*)'))
@then(parsers.re('(?P<user>\w+) deletes files (?P<files>.*)'))
def delete_file(user, files, context):
    multi_file_steps.delete_file(user, files, "client1", context)


@when(parsers.re('(?P<user>\w+) fails to delete files (?P<files>.*)'))
@when(parsers.re('(?P<user>\w+) fails to delete files (?P<files>.*)'))
def delete_file_fail(user, files, context):
    multi_file_steps.delete_file_fail(user, files, "client1", context)


@then(parsers.re('file type of (?P<user>\w+)\'s (?P<file>.*) is (?P<fileType>.*)'))
def check_type(user, file, fileType, context):
    multi_file_steps.check_type(user, file, fileType, "client1", context)


@then(parsers.re('(?P<user>\w+) checks using shell stat if file type of (?P<file>.*) is '
                    '(?P<file_type>.*)'))
def shell_check_type(user, file, file_type, context):
    multi_file_steps.shell_check_type(user, file, file_type, "client1", context)


@then(parsers.re('mode of (?P<user>\w+)\'s (?P<file>.*) is (?P<mode>.*)'))
def check_mode(user, file, mode, context):
    multi_file_steps.check_mode(user, file, mode, "client1", context)


@then(parsers.re('(?P<user>\w+) changes (?P<file>.*) mode to (?P<mode>.*)'))
@when(parsers.re('(?P<user>\w+) changes (?P<file>.*) mode to (?P<mode>.*)'))
def change_mode(user, file, mode, context):
    multi_file_steps.change_mode(user, file, mode, "client1", context)


@then(parsers.re('(?P<user>\w+) fails to change (?P<file>.*) mode to (?P<mode>.*)'))
@when(parsers.re('(?P<user>\w+) fails to change (?P<file>.*) mode to (?P<mode>.*)'))
def change_mode_fail(user, file, mode, context):
    multi_file_steps.change_mode_fail(user, file, mode, "client1", context)


@when(parsers.re('size of (?P<user>\w+)\'s (?P<file>.*) is (?P<size>.*) bytes'))
@then(parsers.re('size of (?P<user>\w+)\'s (?P<file>.*) is (?P<size>.*) bytes'))
def check_size(user, file, size, context):
    multi_file_steps.check_size(user, file, size, "client1", context)


@then(parsers.re('(?P<time1>.*) time of (?P<user>\w+)\'s (?P<file>.*)'
                 ' is (?P<comparator>.*) than (?P<time2>.*) time'))
@then(parsers.re('(?P<time1>.*) time of (?P<user>\w+)\'s (?P<file>.*)'
                 ' is (?P<comparator>.*) to (?P<time2>.*) time'))
def check_time(user, time1, time2, comparator, file, context):
    multi_file_steps.check_time(user, time1, time2, comparator, file, "client1",
                                context)
