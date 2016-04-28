"""
Author: Tomasz Lichon
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite for CRUD operations on regular files in onedata.
"""
import pytest

from tests.test_common import custom_cucumber_env_dir

from pytest_bdd import scenario

from steps.env_steps import *
from steps.auth_steps import *
from steps.multi_dir_steps import *
from steps.multi_file_steps import *
from steps.multi_reg_file_steps import *
from steps.common import *


@pytest.fixture(scope="module", params=["multiprovider_env.json",
                                        "multiprovider_directio_env.json"])
def env_description_file(request):
    absolute_path = os.path.join(custom_cucumber_env_dir, request.param)
    return absolute_path


@scenario(
    '../features/multiprovider_replication.feature',
    'Create files and see them on external provider'
)
def test_create_and_list(env_description_file):
    pass


@scenario(
    '../features/multiprovider_replication.feature',
    'Create empty file and read it on external provider'
)
def test_create_empty_and_read(env_description_file):
    pass


@scenario(
    '../features/multiprovider_replication.feature',
    'Write to file and check size on remote provider'
)
def test_write_and_check_size(env_description_file):
    pass


@scenario(
    '../features/multiprovider_replication.feature',
    'Write to file and read on remote provider'
)
def test_write_and_read(env_description_file):
    pass


@scenario(
    '../features/multiprovider_replication.feature',
    'Big file transfer with MD5 check'
)
def test_big_transfer_and_md5_check(env_description_file):
    pass


@scenario(
    '../features/multiprovider_replication.feature',
    'Create nonempty file and override its contents on remote provider'
)
def test_remote_file_override(env_description_file):
    pass


@scenario(
    '../features/multiprovider_replication.feature',
    'Create nonempty file and remove it on remote provider'
)
def test_remote_file_removal(env_description_file):
    pass


@scenario(
    '../features/multiprovider_replication.feature',
    'Create nonempty file, append remotely, append locally and read both'
)
def test_sequential_appends(env_description_file):
    pass


@scenario(
    '../features/multiprovider_replication.feature',
    'Concurrently write disjoint ranges and read the same on both providers'
)
def test_conflict_on_disjoint_blocks(env_description_file):
    pass
