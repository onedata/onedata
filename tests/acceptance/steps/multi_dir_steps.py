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
import errno


@when(parsers.re('(?P<user>\w+) creates directories (?P<dirs>.*) on (?P<client_node>.*)'))
@when(parsers.re('(?P<user>\w+) creates directories (?P<dirs>.*)\non (?P<client_node>.*)'))
def create(user, dirs, client_node, context):
    create_base(user, dirs, client_node, context)


@when(parsers.re('(?P<user>\w+) fails to create directories (?P<dirs>.*) on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) fails to create directories (?P<dirs>.*) on (?P<client_node>.*)'))
@when(parsers.re('(?P<user>\w+) fails to create directories (?P<dirs>.*)\non (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) fails to create directories (?P<dirs>.*)\non (?P<client_node>.*)'))
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


@when(parsers.re('(?P<user>\w+) creates structure of (?P<number>.*) nested directories in'
                    ' (?P<root_dir>.*) on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) creates structure of (?P<number>.*) nested directories in'
                    ' (?P<root_dir>.*) on (?P<client_node>.*)'))
def create_nested_dirs(user, number, root_dir, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)

    dir_path = client.absolute_path(root_dir)
    for i in range(int(number)):
        dir_path = os.path.join(dir_path, str(i))

    def condition():
        mkdir(client, dir_path, recursive=True)

    assert_(client.perform, condition)


@when(parsers.re('(?P<user>\w+) creates directory and parents (?P<paths>.*) on (?P<client_node>.*)'))
@when(parsers.re('(?P<user>\w+) creates directory and parents (?P<paths>.*)\non (?P<client_node>.*)'))
def create_parents(user, paths, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    paths = list_parser(paths)

    for path in paths:
        dir_path = client.absolute_path(path)

        def condition():
            mkdir(client, dir_path, recursive=True)

        assert_(client.perform, condition)


@when(parsers.re('(?P<user>\w+) deletes empty directories (?P<dirs>.*) on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) deletes empty directories (?P<dirs>.*) on (?P<client_node>.*)'))
def delete_empty(user, dirs, client_node, context):
    delete_empty_base(user, dirs, client_node, context)


@when(parsers.re('(?P<user>\w+) fails to delete empty directories (?P<dirs>.*) on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) fails to delete empty directories (?P<dirs>.*) on (?P<client_node>.*)'))
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


@when(parsers.re('(?P<user>\w+) deletes non-empty directories (?P<dirs>.*) on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) deletes non-empty directories (?P<dirs>.*) on (?P<client_node>.*)'))
def delete_non_empty(user, dirs, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    dirs = list_parser(dirs)

    for dir in dirs:
        path = client.absolute_path(dir)

        def condition():
            rm(client, path, recursive=True, force=True)

        assert_(client.perform, condition)


@when(parsers.re('(?P<user>\w+) deletes empty directory and parents (?P<paths>.*) on ' +
                    '(?P<client_node>.*)'))
def delete_parents(user, paths, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    paths = list_parser(paths)

    for path in paths:
        dir_path = client.absolute_path(path)

        def condition():
            rmdir(client, dir_path, recursive=True)

        assert_(client.perform, condition)


@when(parsers.re('(?P<user>\w+) copies directory (?P<dir1>.*) to (?P<dir2>.*) on (?P<client_node>.*)'))
def copy_dir(user, dir1, dir2, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    src_path = client.absolute_path(dir1)
    dest_path = client.absolute_path(dir2)

    def condition():
        cp(client, src_path, dest_path, recursive=True)

    assert_(client.perform, condition)


@when(parsers.re('(?P<user>\w+) can\'t list (?P<dir>.*) on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) can\'t list (?P<dir>.*) on (?P<client_node>.*)'))
def cannot_list_dir(user, dir, client_node, context):
    list_dir_base(user, dir, client_node, context, should_fail=True)


@when(parsers.re('(?P<user>\w+) can list (?P<dir>.*) on (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) can list (?P<dir>.*) on (?P<client_node>.*)'))
def list_dir(user, dir, client_node, context):
    list_dir_base(user, dir, client_node, context)


def list_dir_base(user, dir, client_node, context, should_fail=False):
    user = context.get_user(user)
    client = user.get_client(client_node)
    path = client.absolute_path(dir)

    def condition():
        try:
            ls(client, path=path)
        except OSError as ex:
            if ex.errno == errno.EPERM:
                return True if should_fail else False
            raise ex
        else:
            return False if should_fail else True

    assert_generic(client.perform, should_fail, condition)


@when(parsers.re('(?P<user>\w+) lists directory nested on level (?P<level>.*) in'
                    ' (?P<root_dir>.*) on (?P<client_node>.*)'))
@when(parsers.re('(?P<user>\w+) lists directory nested on level (?P<level>.*) in'
                    ' (?P<root_dir>.*) on (?P<client_node>.*)'))
def list_nested_dir(user, level, root_dir, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    dir_path = client.absolute_path(root_dir)

    for i in range(int(level)):
        dir_path = os.path.join(dir_path, str(i))

    def condition():
        ls(client, path=dir_path)

    assert_(client.perform, condition, timeout=0)


