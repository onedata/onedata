"""
Author: Piotr Ociepka
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
    context.space_path = context.mount_path + "/spaces/" + space


@when(parsers.parse('{user} creates directories {dirs}'))
def create(user, dirs, client_id, context):
    dirs = list_parser(dirs)
    for dir in dirs:
        ret = docker.exec_(container=client_id,
                     command=["mkdir", context.mount_path +"/"+ dir])
        save_op_code(context, ret)


@when(parsers.parse('{user} creates directory and parents {paths}'))
def create_parents(user, paths, client_id, context):

    paths = list_parser(paths)
    for path in paths:
        ret = docker.exec_(container=client_id,
                     command=["mkdir", "-p", '/'.join([context.mount_path, path])])
        save_op_code(context, ret)


@when(parsers.parse('{user} deletes empty directories {dirs}'))
def delete_empty(user, dirs, client_id,context):
    dirs = list_parser(dirs)
    for dir in dirs:
        ret = docker.exec_(container=client_id,
                           command=["rmdir", '/'.join([context.mount_path,dir])])
        save_op_code(context, ret)


@when(parsers.parse('{user} deletes non-empty directories {dirs}'))
def delete_non_empty(user, dirs, client_id, context):
    dirs = list_parser(dirs)
    for dir in dirs:
        ret = docker.exec_(container=client_id,
                           command=["rm", "-rf", '/'.join([context.mount_path, dir])])
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
                       command=["cp", "-r", '/'.join([context.mount_path, dir1]),
                                '/'.join([context.mount_path, dir2])])
    save_op_code(context, ret)
