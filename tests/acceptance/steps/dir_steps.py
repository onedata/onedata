"""Module implements pytest-bdd steps for operations on directories.
"""
__author__ = "Jakub Kudzia, Piotr Ociepka"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import multi_dir_steps
from tests.utils.acceptance_utils import *


@when(parsers.re('(?P<user>\w+) creates directories (?P<dirs>.*)'))
def create(user, dirs, context):
    multi_dir_steps.create(user, dirs, "client1", context)


@when(parsers.re('(?P<user>\w+) fails to create directories (?P<dirs>.*)'))
@then(parsers.re('(?P<user>\w+) fails to create directories (?P<dirs>.*)'))
def create(user, dirs, context):
    multi_dir_steps.fail_to_create(user, dirs, "client1", context)


@when(parsers.re('(?P<user>\w+) creates structure of (?P<number>.*) nested '
                 'directories in (?P<root_dir>.*)'))
@then(parsers.re('(?P<user>\w+) creates structure of (?P<number>.*) nested '
                 'directories in (?P<root_dir>.*)'))
def create_nested_dirs(user, number, root_dir, context):
    multi_dir_steps.create_nested_dirs(user, number, root_dir, 'client1', context)


@when(parsers.re('(?P<user>\w+) creates directory and parents (?P<paths>.*)'))
def create_parents(user, paths, context):
    multi_dir_steps.create_parents(user, paths, "client1", context)


@when(parsers.re('(?P<user>\w+) deletes empty directories (?P<dirs>.*)'))
@then(parsers.re('(?P<user>\w+) deletes empty directories (?P<dirs>.*)'))
def delete_empty(user, dirs, context):
    multi_dir_steps.delete_empty(user, dirs, "client1", context)


@when(parsers.re('(?P<user>\w+) fails to delete empty directories (?P<dirs>.*)'))
@then(parsers.re('(?P<user>\w+) fails to delete empty directories (?P<dirs>.*)'))
def fail_to_delete_empty(user, dirs, context):
    multi_dir_steps.fail_to_delete_empty(user, dirs, "client1", context)


@when(parsers.re('(?P<user>\w+) deletes non-empty directories (?P<dirs>.*)'))
def delete_non_empty(user, dirs, context):
    multi_dir_steps.delete_non_empty(user, dirs, "client1", context)


@when(parsers.re('(?P<user>\w+) deletes empty directory and parents (?P<paths>.*)'))
def delete_parents(user, paths, context):
    multi_dir_steps.delete_parents(user, paths, "client1", context)


@when(parsers.re('(?P<user>\w+) copies directory (?P<dir1>.*) to (?P<dir2>.*)'))
def copy_dir(user, dir1, dir2, context):
    multi_dir_steps.copy_dir(user, dir1, dir2, "client1", context)


@when(parsers.re('(?P<user>\w+) can\'t list (?P<dir>.*)'))
@then(parsers.re('(?P<user>\w+) can\'t list (?P<dir>.*)'))
def cannot_list_dir(user, dir, context):
    multi_dir_steps.cannot_list_dir(user, dir, "client1", context)


@when(parsers.re('(?P<user>\w+) can list (?P<dir>.*)'))
@then(parsers.re('(?P<user>\w+) can list (?P<dir>.*)'))
def list_dir(user, dir, context):
    multi_dir_steps.list_dir(user, dir, "client1", context)


@when(parsers.re('(?P<user>\w+) lists directory nested on level (?P<level>.*) in'
                 ' (?P<root_dir>.*)'))
@when(parsers.re('(?P<user>\w+) lists directory nested on level (?P<level>.*) in'
                 ' (?P<root_dir>.*)'))
def list_nested_dir(user, level, root_dir, context):
    multi_dir_steps.list_nested_dir(user, level, root_dir, 'client1', context)
