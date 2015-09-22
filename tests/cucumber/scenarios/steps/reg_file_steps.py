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
