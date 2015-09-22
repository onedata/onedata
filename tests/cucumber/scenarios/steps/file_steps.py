import pytest
from pytest_bdd import (given, when, then)
from pytest_bdd import parsers

from environment import docker, env
from common import list_parser
from common import success


@then(parsers.parse('{file} file type is {fileType}'))
def check_type(file, fileType, client_id, context):
    currFileType = docker.exec_(container=client_id,
                        command=["stat", '/'.join([context.mount_path, file]), "--format=%F"],
                        output=True)
    assert fileType == currFileType


@then(parsers.parse('{file} mode is {mode}'))
def check_mode(file, mode, client_id, context):
    curr_mode = docker.exec_(container=client_id,
                     command=["stat", "--format=%a", '/'.join([context.mount_path, file])],
                     output=True)
    assert mode == curr_mode


@when(parsers.parse('{user} changes {file} mode to {mode}'))
def change_mode(user, file, mode, client_id, context):
    docker.exec_(container=client_id,
                 command=["chmod", mode, '/'.join([context.mount_path, file])])
