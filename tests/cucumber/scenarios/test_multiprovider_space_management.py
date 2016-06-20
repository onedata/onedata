"""Test suite for space management in onedata in multiprovider environment
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
from tests.cucumber.steps.spaces_steps import *
from tests.cucumber.steps.env_steps import *
from tests.cucumber.steps.auth_steps import *
from tests.cucumber.steps.multi_auth_steps import *
from tests.cucumber.steps.user_steps import *
from tests.cucumber.steps.multi_file_steps import *
from tests.cucumber.steps.multi_reg_file_steps import *
from tests.cucumber.steps.multi_dir_steps import *
from tests.utils.path_utils import env_file

from pytest_bdd import scenario
from functools import partial
import pytest


scenario = partial(scenario,
                   '../features/multiprovider_space_management.feature')


@pytest.fixture(scope="module", params=["multiprovider_space_management"])
def env_description_file(request):
    return env_file(CUSTOM_CUCUMBER_ENV_DIR, request.param)


@scenario("User joins unused space supported by not his provider")
def test_join_unused_space_supported_by_other_provider(env_description_file):
    pass


@scenario("User joins already used space supported by not his provider")
def test_join_used_space_supported_by_other_provider(env_description_file):
    pass


@scenario("Unused space supported by second provider")
def test_unused_space_supported_by_second_provider(env_description_file):
    pass


@scenario("Used space supported by second provider")
def test_used_space_supported_by_second_provider(env_description_file):
    pass


@scenario("Remove user from space")
def test_remove_user(env_description_file):
    pass


@scenario('Delete supported space')
def test_delete_supported_space(env_description_file):
    pass


@scenario('Space supported and unsupported')
def test_unsupport_space(env_description_file):
    pass

