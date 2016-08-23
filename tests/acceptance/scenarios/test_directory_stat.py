"""Test suite for reading/changing  metadata of directories in onedata.
"""
__author__ = "Jakub Kudzia, Piotr Ociepka"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests import *
from tests.acceptance.steps.env_steps import *
from tests.acceptance.steps.auth_steps import *
from tests.utils.acceptance_utils import *
from tests.acceptance.steps.dir_steps import *
from tests.acceptance.steps.file_steps import *
from tests.acceptance.steps.reg_file_steps import *
from tests.utils.path_utils import env_file

from functools import partial
from pytest_bdd import scenario
import pytest


@pytest.fixture(scope="module",
                params=["singleprovider_singleclient_directio",
                        "singleprovider_singleclient_proxy"])
def env_description_file(request):
    return env_file(CUSTOM_ACCEPTANCE_ENV_DIR, request.param)


scenario = partial(scenario, '../features/directory_stat.feature')


@scenario('Check file type')
def test_type(env_description_file):
    pass


@scenario('Check default access permissions')
def test_default_access(env_description_file):
    pass


@scenario('Change access permissions')
def test_change_access(env_description_file):
    pass


@scenario('Timestamps at creation')
def test_timestamp(env_description_file):
    pass


@scenario('Update timestamps')
def test_update_timestamp(env_description_file):
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
