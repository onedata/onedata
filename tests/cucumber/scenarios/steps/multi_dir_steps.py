"""
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements pytest-bdd steps for operations on directories.
"""

import pytest
from pytest_bdd import (given, when, then)
from pytest_bdd import parsers
import time

from environment import docker, env
from common import *

@given(parsers.parse('we are in space {space}'))
def goto_space(space, context):
    context.space_path = make_path(context, "spaces/"+space)

@when(parsers.parse('{user} creates directories {dirs}'))
def create(user, dirs, client_id, context):
    dirs = list_parser(dirs)
    for dir in dirs:
        ret = docker.exec_(container=client_id,
                     command=["mkdir", make_path(context, dir)])
        save_op_code(context, ret)


@when(parsers.parse('{user} creates directory and parents {paths}'))
def create_parents(user, paths, client_id, context):

    paths = list_parser(paths)
    for path in paths:
        ret = docker.exec_(container=client_id,
                     command=["mkdir", "-p", make_path(context, path)])
        save_op_code(context, ret)


@when(parsers.parse('{user} deletes empty directories {dirs}'))
def delete_empty(user, dirs, client_id,context):
    dirs = list_parser(dirs)
    for dir in dirs:
        ret = docker.exec_(container=client_id,
                           command=["rmdir", make_path(context, dir)])
        save_op_code(context, ret)


@when(parsers.parse('{user} deletes non-empty directories {dirs}'))
def delete_non_empty(user, dirs, client_id, context):
    dirs = list_parser(dirs)
    for dir in dirs:
        ret = docker.exec_(container=client_id,
                           command=["rm", "-rf", make_path(context, dir)])
        save_op_code(context, ret)


@when(parsers.parse('{user} deletes empty directory and parents {paths}'))
def delete_parents(user, paths, client_id, context):
    paths = list_parser(paths)
    for path in paths:
        ret = docker.exec_(container=client_id,
                           command="cd " + context.mount_path + " && rmdir -p " + str(path))
        save_op_code(context, ret)

@when(parsers.parse('{user} copies directory {dir1} to {dir2}'))
def copy_dir(user, dir1, dir2, client_id, context):
    ret = docker.exec_(container=client_id,
                       command=["cp", "-r", make_path(context, dir1), make_path(context, dir2)])
    save_op_code(context, ret)
