"""Test suite for CRUD operations on regular files in onedata.
"""
__author__ = "Tomasz Lichon"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import pytest
from pytest_bdd import scenario

from tests import *

from tests.utils.cucumber_utils import *
from tests.cucumber.steps.env_steps import *
from tests.cucumber.steps.multi_auth_steps import *
from tests.cucumber.steps.multi_dir_steps import *
from tests.cucumber.steps.multi_file_steps import *
from tests.cucumber.steps.multi_reg_file_steps import *


@pytest.fixture(scope="module", params=["multiprovider_directio_env.json",
                                        "multiprovider_env.json"])
def env_description_file(request):
    absolute_path = os.path.join(CUSTOM_CUCUMBER_ENV_DIR, request.param)
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


@pytest.mark.xfail_env(envs=["multiprovider_directio_env.json",
                               "multiprovider_env.json"],
                       reason="environement synchronization")
@scenario(
    '../features/multiprovider_replication.feature',
    'Create nonempty file, append remotely, append locally and read both'
)
def test_sequential_appends(env_description_file):
    pass


# todo fix environement synchronization
@pytest.mark.xfail_env(envs=["multiprovider_directio_env.json",
                               "multiprovider_env.json"],
                       reason="environement synchronization")
@scenario(
    '../features/multiprovider_replication.feature',
    'Concurrently write disjoint ranges and read the same on both providers'
)
def test_conflict_on_disjoint_blocks(env_description_file):
    pass
