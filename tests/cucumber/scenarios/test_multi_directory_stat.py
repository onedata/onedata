"""
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite for reading/changing  metadata of directories in onedata,
in multi-client environment.
"""

from pytest_bdd import scenario

from steps.env_steps import *
from steps.auth_steps import *
from steps.multi_dir_steps import *
from steps.common import *
from steps.multi_file_steps import *
from steps.multi_reg_file_steps import *


@scenario(
    '../features/multi_directory_stat.feature',
    'Check file type'
)
def test_type(env_description_file):
    pass


@scenario(
    '../features/multi_directory_stat.feature',
    'Check default access permissions'
)
def test_default_access(env_description_file):
    pass


@scenario(
    '../features/multi_directory_stat.feature',
    'Change access permissions'
)
def test_change_access(env_description_file):
    pass


@scenario(
    '../features/multi_directory_stat.feature',
    'Change someone\'s file access permissions'
)
def test_change_access_someone(env_description_file):
    pass


@scenario(
    '../features/multi_directory_stat.feature',
    'Timestamps at creation'
)
def test_timestamp(env_description_file):
    pass


# TODO VFS-1506
# @scenario(
#     '../features/multi_directory_stat.feature',
#     'Update timestamps without write permission'
# )
# def test_update_timestamp_without_permission(env_description_file):
#     pass


@scenario(
    '../features/multi_directory_stat.feature',
    'Update timestamps with write permission'
)
def test_update_timestamp_with_permission(env_description_file):
    pass


@scenario(
    '../features/multi_directory_stat.feature',
    'Access time'
)
def test_access_time(env_description_file):
    pass


@scenario(
    '../features/multi_directory_stat.feature',
    'Modification time'
)
def test_modification_time(env_description_file):
    pass


@scenario(
    '../features/multi_directory_stat.feature',
    'Status-change time when changing mode'
)
def test_stat_change_time_chmod(env_description_file):
    pass


@scenario(
    '../features/multi_directory_stat.feature',
    'Status-change time when renaming'
)
def test_stat_change_time_mv(env_description_file):
    pass
