"""
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite for reading/changing  metadata of regular files in onedata,
in multi-client environment.
"""

from pytest_bdd import scenario

from steps.env_steps import *
from steps.multi_auth_steps import *
from steps.multi_dir_steps import *
from steps.cucumber_utils import *
from steps.multi_file_steps import *
from steps.multi_reg_file_steps import *


@scenario(
    '../features/multi_reg_file_stat.feature',
    'Check file type when empty'
)
def test_type_empty(env_description_file):
    pass


@scenario(
    '../features/multi_reg_file_stat.feature',
    'Check file type when non-empty'
)
def test_type(env_description_file):
    pass


@scenario(
    '../features/multi_reg_file_stat.feature',
    'Check default access permissions'
)
def test_default_access(env_description_file):
    pass


@scenario(
    '../features/multi_reg_file_stat.feature',
    'Change access permissions'
)
def test_change_access(env_description_file):
    pass


@scenario(
    '../features/multi_reg_file_stat.feature',
    'Increase regular file size'
)
def test_increase_size(env_description_file):
    pass


@scenario(
    '../features/multi_reg_file_stat.feature',
    'Decrease regular file size'
)
def test_decrease_size(env_description_file):
    pass


@scenario(
    '../features/multi_reg_file_stat.feature',
    'Truncate regular file without write permission'
)
def test_truncate_without_permission(env_description_file):
    pass


@scenario(
    '../features/multi_reg_file_stat.feature',
    'Timestamps at creation'
)
def test_timestamp(env_description_file):
    pass


# # TODO VFS-1506
@pytest.mark.xfail_env(
        envs=["env", "env2", "env3"],
        reason="touch on file without write permission should fail, "
               "it will be checked in VFS-1506")
@scenario(
    '../features/multi_reg_file_stat.feature',
    'Update timestamps without write permission',
)
def test_update_timestamp_without_permission(env_description_file):
    pass


@scenario(
    '../features/multi_reg_file_stat.feature',
    'Update timestamps with write permission'
)
def test_update_timestamp_with_permission(env_description_file):
    pass


# # TODO VFS-1822
@pytest.mark.xfail_env(
        envs=["env", "env2", "env3"],
        reason="reading fails in oneclient")
@scenario(
    '../features/multi_reg_file_stat.feature',
    'Access time'
)
def test_access_time(env_description_file):
    pass


@scenario(
    '../features/multi_reg_file_stat.feature',
    'Modification time'
)
def test_modification_time(env_description_file):
    pass


# TODO VFS-1821
@pytest.mark.xfail_env(
        envs=["env", "env2", "env3"],
        reason="status-change times is equal to access and modification, "
               "it will be checked VFS-1821")
@scenario(
    '../features/multi_reg_file_stat.feature',
    'Status-change time when changing mode'
)
def test_stat_change_time_chmod(env_description_file):
    pass


@scenario(
    '../features/multi_reg_file_stat.feature',
    'Status-change time when renaming'
)
def test_stat_change_time_mv(env_description_file):
    pass
