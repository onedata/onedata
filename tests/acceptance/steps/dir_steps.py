"""Module implements pytest-bdd steps for operations on directories.
"""
__author__ = "Jakub Kudzia, Piotr Ociepka"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import multi_dir_steps
from tests.utils.acceptance_utils import *


@when(parsers.parse('{user} creates directories {dirs}'))
def create(user, dirs, context):
    multi_dir_steps.create(user, dirs, "client1", context)


@when(parsers.parse('{user} fails to create directories {dirs}'))
@then(parsers.parse('{user} fails to create directories {dirs}'))
def create(user, dirs, context):
    multi_dir_steps.fail_to_create(user, dirs, "client1", context)


@when(parsers.parse('{user} creates structure of {number} nested directories in {root_dir}'))
@then(parsers.parse('{user} creates structure of {number} nested directories in {root_dir}'))
def create_nested_dirs(user, number, root_dir, context):
    multi_dir_steps.create_nested_dirs(user, number, root_dir, 'client1', context)


@when(parsers.parse('{user} creates directory and parents {paths}'))
def create_parents(user, paths, context):
    multi_dir_steps.create_parents(user, paths, "client1", context)


@when(parsers.parse('{user} deletes empty directories {dirs}'))
@then(parsers.parse('{user} deletes empty directories {dirs}'))
def delete_empty(user, dirs, context):
    multi_dir_steps.delete_empty(user, dirs, "client1", context)


@when(parsers.parse('{user} fails to delete empty directories {dirs}'))
@then(parsers.parse('{user} fails to delete empty directories {dirs}'))
def fail_to_delete_empty(user, dirs, context):
    multi_dir_steps.fail_to_delete_empty(user, dirs, "client1", context)


@when(parsers.parse('{user} deletes non-empty directories {dirs}'))
def delete_non_empty(user, dirs, context):
    multi_dir_steps.delete_non_empty(user, dirs, "client1", context)


@when(parsers.parse('{user} deletes empty directory and parents {paths}'))
def delete_parents(user, paths, context):
    multi_dir_steps.delete_parents(user, paths, "client1", context)


@when(parsers.parse('{user} copies directory {dir1} to {dir2}'))
def copy_dir(user, dir1, dir2, context):
    multi_dir_steps.copy_dir(user, dir1, dir2, "client1", context)


@when(parsers.parse('{user} can\'t list {dir}'))
@then(parsers.parse('{user} can\'t list {dir}'))
def cannot_list_dir(user, dir, context):
    multi_dir_steps.cannot_list_dir(user, dir, "client1", context)


@when(parsers.parse('{user} lists {dir}'))
@then(parsers.parse('{user} lists {dir}'))
@when(parsers.parse('{user} can list {dir}'))
@then(parsers.parse('{user} can list {dir}'))
def list_dir(user, dir, context):
    multi_dir_steps.list_dir(user, dir, "client1", context)


@when(parsers.parse('{user} lists directory nested on level {level} in'
                    ' {root_dir}'))
@when(parsers.parse('{user} lists directory nested on level {level} in'
                    ' {root_dir}'))
def list_nested_dir(user, level, root_dir, context):
    multi_dir_steps.list_nested_dir(user, level, root_dir, 'client1', context)
