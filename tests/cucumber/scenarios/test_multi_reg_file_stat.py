"""Test suite for reading/changing  metadata of regular files in onedata,
in multi-client environment.
"""

__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in "\
              "LICENSE.txt"

from tests.cucumber.steps.cucumber_utils import *
from tests.cucumber.steps.env_steps import *
from tests.cucumber.steps.multi_auth_steps import *
from tests.cucumber.steps.multi_dir_steps import *
from tests.cucumber.steps.multi_file_steps import *
from tests.cucumber.steps.multi_reg_file_steps import *

from pytest_bdd import scenario
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


@scenario('Increase regular file size')
def test_increase_size(env_description_file):
    pass


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


@scenario('Status-change time when renaming')
def test_stat_change_time_mv(env_description_file):
    pass
