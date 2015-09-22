import pytest
from pytest_bdd import (given, when, then)
from pytest_bdd import parsers
import time

from environment import docker, env
from common import *

@given(parsers.parse('we are in space {space}'))
def goto_space(space, context):
    #nie ma sensu robic cd bo docker.exec i tak startuje z domyslnego katalogu
    context.space_path = context.mount_path + "/spaces/" + space


@when(parsers.parse('{user} creates directories {dirs} and {foo}'))
def create(user, dirs, client_id, context, foo):
    # if foo == 'true':
    #     time.sleep(600)
    dirs = list_parser(dirs)
    print "START: "
    print docker.exec_(container=client_id,
                     command=["ls", context.mount_path], output=True)
    print "START2: "
    print docker.exec_(container=client_id,
                     command=["ls", "/root"], output=True)

    for dir in dirs:
        ret = docker.exec_(container=client_id,
                     command=["mkdir", context.mount_path +"/"+ dir])
        print "RETURN: " + str(ret)
        save_op_code(context, ret)
    # time.sleep(600)


@when(parsers.parse('{user} creates directory and parents {paths}'))
def create_parents(user, paths, client_id, context):

    print "START: "
    print docker.exec_(container=client_id,
                     command=["ls", context.mount_path])
    print "START: 2"
    print docker.exec_(container=client_id,
                     command=["ls", "/root"])

    paths = list_parser(paths)
    for path in paths:
        ret = docker.exec_(container=client_id,
                     command=["mkdir", "-p", '/'.join([context.mount_path, path])])
        save_op_code(context, ret)


@when(parsers.parse('{user} renames {dir1} to {dir2}'))
def rename(user, dir1, dir2, client_id, context):
    print "TESTRENAME_START: "
    print docker.exec_(container=client_id, command="ls " + context.mount_path, output=True)
    print "TEST2: "
    print docker.exec_(container=client_id, command="ls " + context.mount_path + "/spaces/s1",
                       output=True)

    # time.sleep(600)
    ret = docker.exec_(container=client_id,
                 command=["mv", '/'.join([context.mount_path, dir1]),
                          '/'.join([context.mount_path, dir2])])

    save_op_code(context, ret)

    print "TESTRENAME_AFTER: "
    print docker.exec_(container=client_id, command="ls " + context.mount_path, output=True)
    print "TEST2: "
    print docker.exec_(container=client_id, command="ls " + context.mount_path + "/spaces/s1",
                       output=True)


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


@then(parsers.parse('{dirs} are in ls {path}'))
def ls_present(dirs, path, client_id, context):
    cmd = ["ls", context.mount_path + "/" + path]
    ls_dirs = docker.exec_(container=client_id, command=cmd, output=True).split()
    dirs = list_parser(dirs)
    for dir in dirs:
        assert dir in ls_dirs


@then(parsers.parse('{dirs} are not in ls {path}'))
def ls_absent(dirs, path, client_id, context):
    cmd = ["ls", context.mount_path + "/" + path]
    ls_dirs = docker.exec_(container=client_id, command=cmd, output=True).split()
    dirs = list_parser(dirs)
    for dir in dirs:
        assert dir not in ls_dirs


@then("clean succeeds")
def clean(client_id, context):
    # time.sleep(600)
    print "CLEAN_START: "
    print docker.exec_(container=client_id, command="ls " + context.mount_path, output=True)
    print "CLEAN_START2: "
    print docker.exec_(container=client_id, command="ls -a " + context.mount_path + "/spaces/s1",
                       output=True)

    spaces = docker.exec_(container=client_id,
                          command=['ls', context.mount_path + '/spaces'],
                          output=True)

    spaces = spaces.split("\n")

    #clean spaces
    for space in spaces:
        ret = docker.exec_(container=client_id,
                     command="rm -rf " + '/'.join([context.mount_path, 'spaces', str(space), '*']))
        save_op_code(context, ret)

    # ####################################################################
    # # print "CHECK: " + docker.exec_(container=client_id,
    # #                    command="ps aux | grep './oneclient --authentication token' | " +
    # #                    "grep -v 'grep'", output=True)
    # pid = docker.exec_(container=client_id,
    #                    command="ps aux | grep './oneclient --authentication token' | " +
    #                    "grep -v 'grep' | awk '{print $2}'", output=True)
    # # print "PID: " + pid
    # # print type(pid)
    # cmd = "kill -KILL " + str(pid)
    # print "CMD: " + cmd
    # # time.sleep(600)
    # print "KILL: " + docker.exec_(container=client_id, command=cmd, output=True)
    # docker.exec_(container=client_id, command="/bin/fusermount -u " + context.mount_path)
    # # remove onedata dir
    # ret = docker.exec_(container=client_id, command="rm -rf " + context.mount_path)
    # save_op_code(context, ret)
    # ##################################################################################



    print "CLEAN: "
    print docker.exec_(container=client_id, command="ls -l /root", output=True)
    print "CLEAN2: "
    print docker.exec_(container=client_id, command="ls -l " + context.mount_path + "/spaces",
                       output=True)
    success(client_id, context)

