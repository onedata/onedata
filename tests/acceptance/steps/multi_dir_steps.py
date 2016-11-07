"""Module implements pytest-bdd steps for operations on directories in
multiclient environment.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.utils.utils import assert_generic, assert_
from tests.utils.acceptance_utils import *
from tests.utils.client_utils import ls, rm, rmdir, mkdir, cp
import os


@when(parsers.parse('{user} creates directories {dirs} on {client_node}'))
@when(parsers.parse('{user} creates directories {dirs}\non {client_node}'))
def create(user, dirs, client_node, context):
    create_base(user, dirs, client_node, context)


@when(parsers.parse('{user} fails to create directories {dirs} on {client_node}'))
@then(parsers.parse('{user} fails to create directories {dirs} on {client_node}'))
@when(parsers.parse('{user} fails to create directories {dirs}\non {client_node}'))
@then(parsers.parse('{user} fails to create directories {dirs}\non {client_node}'))
def fail_to_create(user, dirs, client_node, context):
    create_base(user, dirs, client_node, context, should_fail=True)


def create_base(user, dirs, client_node, context, should_fail=False):
    dirs = list_parser(dirs)
    user = context.get_user(user)
    client = user.get_client(client_node)

    for dir in dirs:
        path = client.absolute_path(dir)

        def condition():
            mkdir(client, path)
        assert_generic(client.perform, should_fail, condition)


@when(parsers.parse('{user} creates structure of {number} nested directories in'
                    ' {root_dir} on {client_node}'))
@then(parsers.parse('{user} creates structure of {number} nested directories in'
                    ' {root_dir} on {client_node}'))
def create_nested_dirs(user, number, root_dir, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)

    dir_path = client.absolute_path(root_dir)
    for i in range(int(number)):
        dir_path = os.path.join(dir_path, str(i))

    def condition():
        mkdir(client, dir_path, recursive=True)

    assert_(client.perform, condition)


@when(parsers.parse('{user} creates directory and parents {paths} on {client_node}'))
@when(parsers.parse('{user} creates directory and parents {paths}\non {client_node}'))
def create_parents(user, paths, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    paths = list_parser(paths)

    for path in paths:
        dir_path = client.absolute_path(path)

        def condition():
            mkdir(client, dir_path, recursive=True)

        assert_(client.perform, condition)


@when(parsers.parse('{user} deletes empty directories {dirs} on {client_node}'))
@then(parsers.parse('{user} deletes empty directories {dirs} on {client_node}'))
def delete_empty(user, dirs, client_node, context):
    delete_empty_base(user, dirs, client_node, context)


@when(parsers.parse('{user} fails to delete empty directories {dirs} on {client_node}'))
@then(parsers.parse('{user} fails to delete empty directories {dirs} on {client_node}'))
def fail_to_delete_empty(user, dirs, client_node, context):
    delete_empty_base(user, dirs, client_node, context, should_fail=True)


def delete_empty_base(user, dirs, client_node, context, should_fail=False):
    user = context.get_user(user)
    client = user.get_client(client_node)
    dirs = list_parser(dirs)

    for dir in dirs:
        path = client.absolute_path(dir)

        def condition():
            rmdir(client, path)

        assert_generic(client.perform, should_fail, condition)


@when(parsers.parse('{user} deletes non-empty directories {dirs} on {client_node}'))
@then(parsers.parse('{user} deletes non-empty directories {dirs} on {client_node}'))
def delete_non_empty(user, dirs, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    dirs = list_parser(dirs)

    for dir in dirs:
        path = client.absolute_path(dir)

        def condition():
            rm(client, path, recursive=True, force=True)

        assert_(client.perform, condition)


@when(parsers.parse('{user} deletes empty directory and parents {paths} on ' +
                    '{client_node}'))
def delete_parents(user, paths, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    paths = list_parser(paths)

    for path in paths:
        dir_path = client.absolute_path(path)

        def condition():
            rmdir(client, dir_path, recursive=True)

        assert_(client.perform, condition)


@when(parsers.parse('{user} copies directory {dir1} to {dir2} on {client_node}'))
def copy_dir(user, dir1, dir2, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    src_path = client.absolute_path(dir1)
    dest_path = client.absolute_path(dir2)

    def condition():
        cp(client, src_path, dest_path, recursive=True)

    assert_(client.perform, condition)


@when(parsers.parse('{user} can\'t list {dir} on {client_node}'))
@then(parsers.parse('{user} can\'t list {dir} on {client_node}'))
def cannot_list_dir(user, dir, client_node, context):
    list_dir_base(user, dir, client_node, context, should_fail=True)


@when(parsers.parse('{user} can list {dir} on {client_node}'))
@then(parsers.parse('{user} can list {dir} on {client_node}'))
def list_dir(user, dir, client_node, context):
    list_dir_base(user, dir, client_node, context)


def list_dir_base(user, dir, client_node, context, should_fail=False):
    user = context.get_user(user)
    client = user.get_client(client_node)
    path = client.absolute_path(dir)

    def condition():
        ls(client, path=path)

    assert_generic(client.perform, should_fail, condition)


@when(parsers.parse('{user} lists directory nested on level {level} in'
                    ' {root_dir} on {client_node}'))
@when(parsers.parse('{user} lists directory nested on level {level} in'
                    ' {root_dir} on {client_node}'))
def list_nested_dir(user, level, root_dir, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    dir_path = client.absolute_path(root_dir)

    for i in range(int(level)):
        dir_path = os.path.join(dir_path, str(i))

    def condition():
        ls(client, path=dir_path)

    assert_(client.perform, condition, timeout=0)


