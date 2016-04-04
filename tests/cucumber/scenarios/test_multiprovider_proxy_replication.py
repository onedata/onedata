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


@scenario(
    '../features/multiprovider_proxy_replication.feature',
    'Write to file and check size on remote provider'
)
def test_write_and_check_size():
    pass


@scenario(
    '../features/multiprovider_proxy_replication.feature',
    'Write to file and read on remote provider'
)
def test_write_and_read():
    pass


@scenario(
    '../features/multiprovider_proxy_replication.feature',
    'Big file transfer with MD5 check'
)
def test_big_transfer_and_md5_check():
    pass


@scenario(
    '../features/multiprovider_proxy_replication.feature',
    'Remote file override'
)
def test_remote_file_override():
    pass


@scenario(
    '../features/multiprovider_proxy_replication.feature',
    'Remote file removal'
)
def test_remote_file_removal():
    pass


@scenario(
    '../features/multiprovider_proxy_replication.feature',
    'Sequential appends'
)
def test_sequential_appends():
    pass

# todo fix python syntax
# @scenario(
#     '../features/multiprovider_proxy_replication.feature',
#     'Conflict on disjoint blocks'
# )
# def test_conflict_on_disjoint_blocks():
#     pass
