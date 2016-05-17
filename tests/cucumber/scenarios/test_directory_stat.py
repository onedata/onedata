"""
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite for reading/changing  metadata of directories in onedata.
"""
from tests import *
from tests.cucumber.steps.env_steps import *
from tests.cucumber.steps.auth_steps import *
from tests.cucumber.steps.cucumber_utils import *
from tests.cucumber.steps.dir_steps import *
from tests.cucumber.steps.file_steps import *
from tests.cucumber.steps.reg_file_steps import *

from pytest_bdd import scenario
import pytest


@pytest.fixture(scope="module",
                params=["singleprovider_singleclient_directio.json",
                        "singleprovider_singleclient_proxy.json"])
def env_description_file(request):
    absolute_path = os.path.join(CUSTOM_CUCUMBER_ENV_DIR, request.param)
    return absolute_path


@scenario(
    '../features/directory_stat.feature',
    'Check file type'
)
def test_type(env_description_file):
    pass


@scenario(
    '../features/directory_stat.feature',
    'Check default access permissions'
)
def test_default_access(env_description_file):
    pass


@scenario(
    '../features/directory_stat.feature',
    'Change access permissions'
)
def test_change_access(env_description_file):
    pass


@scenario(
    '../features/directory_stat.feature',
    'Timestamps at creation'
)
def test_timestamp(env_description_file):
    pass


@scenario(
    '../features/directory_stat.feature',
    'Update timestamps'
)
def test_update_timestamp(env_description_file):
    pass


@scenario(
    '../features/directory_stat.feature',
    'Access time'
)
def test_access_time(env_description_file):
    pass


@scenario(
    '../features/directory_stat.feature',
    'Modification time'
)
def test_modification_time(env_description_file):
    pass


# TODO VFS-1821
@pytest.mark.xfail_env(
        envs=["singleprovider_singleclient_directio",
              "singleprovider_singleclient_proxy"],
        reason="status-change times is equal to access and modification")
@scenario(
    '../features/directory_stat.feature',
    'Status-change time when changing mode'
)
def test_stat_change_time_chmod(env_description_file):
    pass


@scenario(
    '../features/directory_stat.feature',
    'Status-change time when renaming'
)
def test_stat_change_time_mv(env_description_file):
    pass
