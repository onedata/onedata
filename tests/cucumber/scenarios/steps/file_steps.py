import pytest
from pytest_bdd import (given, when, then)
from pytest_bdd import parsers

from environment import docker, env
from common import list_parser
from common import success

@then(parsers.parse('{file} file type is {fileType}'))
def check_type(file, fileType, client_id):
    currFileType = docker.exec_(container=client_id, command=["stat", file, "--format=%F"], output=True)
    print "CURRFILETYPE: " + currFileType
    print "FILETYPE: " + fileType
    print fileType == currFileType
    assert fileType == currFileType

@when(parsers.parse('{user} changes {file} mode to {mod}'))
def change_mode(user, file, mod, client_id):
    docker.exec_(container=client_id, command=["chmod", mod, file])
