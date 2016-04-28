"""
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements pytest-bdd steps for operations on directories in multiclient environment.
"""

import pytest
from pytest_bdd import (given, when, then)
from pytest_bdd import parsers
import time

from environment import docker, env
from common import *


@when(parsers.parse('{user} creates directories {dirs} on {client_node}'))
@when(parsers.parse('{user} creates directories {dirs}\non {client_node}'))
def create(user, dirs, client_node, context):
    dirs = list_parser(dirs)
    client = get_client(client_node, user, context)
    for dir in dirs:
        path = make_path(dir, client)
        return_code = mkdir(client, path, user=user)
        save_op_code(context, user, return_code)


@when(parsers.parse('{user} creates directory and parents {paths} on {client_node}'))
@when(parsers.parse('{user} creates directory and parents {paths}\non {client_node}'))
def create_parents(user, paths, client_node, context):
    client = get_client(client_node, user, context)
    paths = list_parser(paths)
    for path in paths:
        return_code = mkdir(client, make_path(path, client), recursive=True, user=user)
        save_op_code(context, user, return_code)


@when(parsers.parse('{user} deletes empty directories {dirs} on {client_node}'))
def delete_empty(user, dirs, client_node, context):
    client = get_client(client_node, user, context)
    dirs = list_parser(dirs)
    for dir in dirs:
        path = make_path(dir, client)
        ret = rmdir(client, path, user=user)
        save_op_code(context, user, ret)


@when(parsers.parse('{user} deletes non-empty directories {dirs} on {client_node}'))
def delete_non_empty(user, dirs, client_node, context):
    client = get_client(client_node, user, context)
    dirs = list_parser(dirs)
    for dir in dirs:
        path = make_path(dir, client)
        ret = rm(client, path, recursive=True, force=True, user=user)
        save_op_code(context, user, ret)


@when(parsers.parse('{user} deletes empty directory and parents {paths} on ' +
                    '{client_node}'))
def delete_parents(user, paths, client_node, context):
    client = get_client(client_node, user, context)
    paths = list_parser(paths)
    for path in paths:
        ret = rmdir(client, str(path), recursive=True,
                    from_path=client.mount_path, user=user)
        save_op_code(context, user, ret)


@when(parsers.parse('{user} copies directory {dir1} to {dir2} on {client_node}'))
def copy_dir(user, dir1, dir2, client_node, context):
    client = get_client(client_node, user, context)
    src_path = make_path(dir1, client)
    dest_path = make_path(dir2, client)
    ret = cp(client, src_path, dest_path, recursive=True, user=user)
    save_op_code(context, user, ret)


@when(parsers.parse('{user} can\'t list {dir} on {client_node}'))
@then(parsers.parse('{user} can\'t list {dir} on {client_node}'))
def list_dir(user, dir, client_node, context):
    client = get_client(client_node, user, context)
    path = make_path(dir, client)

    def condition():
        try:
            ls(client, user=user, path=path)
            return False
        except:
            return True

    assert repeat_until(condition, timeout=10)
