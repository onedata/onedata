"""
Author: Tomasz Lichon
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite for CRUD operations on regular files in onedata.
"""

from pytest_bdd import scenario

from steps.env_steps import *
from steps.auth_steps import *
from steps.multi_dir_steps import *
from steps.multi_file_steps import *
from steps.multi_reg_file_steps import *
from steps.common import *


@scenario(
    '../features/multiprovider_proxy_replication.feature',
    'Create files and see them on external provider'
)
def test_create_and_list():
    pass


@scenario(
    '../features/multiprovider_proxy_replication.feature',
    'Create empty file and read it on external provider'
)
def test_create_empty_and_read():
    pass
