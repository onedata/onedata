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


# @when(parsers.parse('{user} renames {dir1} to {dir2}'))
# def rename(user, dir1, dir2, client_id, context):
#
#     ret = docker.exec_(container=client_id,
#                        command=["mv", '/'.join([context.mount_path, dir1]),
#                                 '/'.join([context.mount_path, dir2])])
#     save_op_code(context, ret)


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


# @then(parsers.parse('{dirs} are in ls {path}'))
# def ls_present(dirs, path, client_id, context):
#     cmd = ["ls", context.mount_path + "/" + path]
#     ls_dirs = docker.exec_(container=client_id, command=cmd, output=True).split()
#     dirs = list_parser(dirs)
#     for dir in dirs:
#         assert dir in ls_dirs
#
#
# @then(parsers.parse('{dirs} are not in ls {path}'))
# def ls_absent(dirs, path, client_id, context):
#     cmd = ["ls", context.mount_path + "/" + path]
#     ls_dirs = docker.exec_(container=client_id, command=cmd, output=True).split()
#     dirs = list_parser(dirs)
#     for dir in dirs:
#         assert dir not in ls_dirs
