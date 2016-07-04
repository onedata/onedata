"""Test suite for reading/changing  metadata of regular files in onedata.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests import *
from tests.cucumber.steps.auth_steps import *
from tests.cucumber.steps.env_steps import *
from tests.utils.cucumber_utils import *
from tests.cucumber.steps.dir_steps import *
from tests.cucumber.steps.file_steps import *
from tests.cucumber.steps.reg_file_steps import *
from tests.utils.path_utils import env_file

from pytest_bdd import scenario
import pytest


@pytest.fixture(scope="module",
                params=["singleprovider_singleclient_directio",
                        "singleprovider_singleclient_proxy"])
def env_description_file(request):
    return env_file(CUSTOM_CUCUMBER_ENV_DIR, request.param)


@scenario(
    '../features/reg_file_stat.feature',
    'Check file type when empty'
)
def test_type_empty(env_description_file):
    pass


@scenario(
    '../features/reg_file_stat.feature',
    'Check file type when non-empty'
)
def test_type(env_description_file):
    pass


@scenario(
    '../features/reg_file_stat.feature',
    'Check default access permissions'
)
def test_default_access(env_description_file):
    pass


@scenario(
    '../features/reg_file_stat.feature',
    'Change access permissions'
)
def test_change_access(env_description_file):
    pass


@scenario(
    '../features/reg_file_stat.feature',
    'Increase regular file size'
)
def test_increase_size(env_description_file):
    pass


@scenario(
    '../features/reg_file_stat.feature',
    'Decrease regular file size'
)
def test_decrease_size(env_description_file):
    pass


@scenario(
    '../features/reg_file_stat.feature',
    'Timestamps at creation'
)
def test_timestamp(env_description_file):
    pass


@scenario(
    '../features/reg_file_stat.feature',
    'Update timestamps'
)
def test_update_timestamp(env_description_file):
    pass


@scenario(
    '../features/reg_file_stat.feature',
    'Access time'
)
def test_access_time(env_description_file):
    pass


@scenario(
    '../features/reg_file_stat.feature',
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
    '../features/reg_file_stat.feature',
    'Status-change time when changing mode'
)
def test_stat_change_time_chmod(env_description_file):
    pass


@scenario(
    '../features/reg_file_stat.feature',
    'Status-change time when renaming'
)
def test_stat_change_time_mv(env_description_file):
    pass
