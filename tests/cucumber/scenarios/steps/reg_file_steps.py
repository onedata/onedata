import pytest
from pytest_bdd import (given, when, then)
from pytest_bdd import parsers
import time

from environment import docker, env
from common import *


@when(parsers.parse('{user} creates regular files {files}'))
def create_reg_file(user, files, client_id, context):
    files = list_parser(files)

    time.sleep(600)

    for file in files:
        ret = docker.exec_(container=client_id,
                     command=["touch", context.mount_path +"/"+ file])
        save_op_code(context, ret)


@when(parsers.parse('{user} writes "{text}" to {file}'))
def write(user, text, file, context, client_id):
    # UWAGA, moze byc problem z kodowaniem argumentu text, unicode lub string type
    ret = docker.exec_(container=client_id, command=["echo", text, " > ", file])
    save_op_code(context, ret)


@then(parsers.parse('{user} reads "{text}" from {file}'))
def read(user, text, file, context, client_id):
    read_text = docker.exec_(container=client_id, command=["cat", file], output=True)
    assert read_text == text


@when(parsers.parse('{user} copies regular file {file1} to {file2}'))
def copy_reg_file(user, file1, file2, context):
    ret = docker.exec_(container=client_id,
                       command=["cp", '/'.join([context.mount_path, file1]),
                                '/'.join([context.mount_path, file2])])
    save_op_code(context, ret)


@when(parsers.parse('{user} changes {file} size to {new_size} bytes'))
def truncate(user, file, new_size, context):
    ret = docker.exec_(container=client_id,
                       command=["truncate", "--size="+str(new_size),
                                '/'.join([context.mount_path, file])])
    save_op_code(context, ret)

