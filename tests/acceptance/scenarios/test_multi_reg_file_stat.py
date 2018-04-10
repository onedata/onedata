"""Test suite for reading/changing  metadata of regular files in onedata,
in multi-client environment.
"""

__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in "\
              "LICENSE.txt"

from tests.utils.acceptance_utils import *
from tests.acceptance.steps.env_steps import *
from tests.acceptance.steps.multi_auth_steps import *
from tests.acceptance.steps.multi_dir_steps import *
from tests.acceptance.steps.multi_file_steps import *
from tests.acceptance.steps.multi_reg_file_steps import *

from pytest_bdd import scenario
import pytest
from functools import partial


scenario = partial(scenario, '../features/multi_reg_file_stat.feature')


@scenario('Check file type when empty')
def test_type_empty(env_description_file):
    pass


@scenario('Check file type when non-empty')
def test_type(env_description_file):
    pass


@scenario('Check default access permissions')
def test_default_access(env_description_file):
    pass


@scenario('Change access permissions')
def test_change_access(env_description_file):
    pass


@pytest.mark.xfail_env(
    envs=["singleprovider_multiclient_directio",
          "singleprovider_multiclient_proxy",
          "multiprovider_proxy",
          "multiprovider_directio"],
    reason="stat returns size 0")
@scenario('Increase regular file size')
def test_increase_size(env_description_file):
    pass


@pytest.mark.xfail_env(
    envs=["singleprovider_multiclient_directio",
          "singleprovider_multiclient_proxy",
          "multiprovider_proxy",
          "multiprovider_directio"],
    reason="stat returns size 0")
@scenario('Decrease regular file size')
def test_decrease_size(env_description_file):
    pass


@scenario('Truncate regular file without write permission')
def test_truncate_without_permission(env_description_file):
    pass


@scenario('Timestamps at creation')
def test_timestamp(env_description_file):
    pass


@scenario('Update timestamps without write permission',)
def test_update_timestamp_without_permission(env_description_file):
    pass


@scenario('Update timestamps with write permission')
def test_update_timestamp_with_permission(env_description_file):
    pass


@scenario('Access time')
def test_access_time(env_description_file):
    pass


@scenario('Modification time')
def test_modification_time(env_description_file):
    pass


@scenario('Status-change time when changing mode')
def test_stat_change_time_chmod(env_description_file):
    pass


@pytest.mark.skip_env(
    envs=["multiprovider_proxy_s3"],
    reason="valid only on posix storage"
)
@scenario('Status-change time when renaming on posix storage')
def test_stat_change_time_mv_on_posix(env_description_file):
    pass


# @pytest.mark.skip_env(
#     envs=[
#         "multiprovider_directio",
#         "multiprovider_proxyio",
#         "multiprovider_proxy",
#         "singleprovider_multiclient_proxyio",
#         "singleprovider_multiclient_directio"
#     ],
#     reason="valid only on nonposix storage"
# )
@pytest.mark.skip
@scenario('Status-change time when renaming on nonposix storage')
def test_stat_change_time_mv_on_nonposix(env_description_file):
    pass
