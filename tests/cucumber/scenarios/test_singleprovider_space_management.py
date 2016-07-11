"""Test suite for space management in onedata in singleprovider environment
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
from tests.utils.cucumber_utils import *
from tests.utils.path_utils import env_file

from pytest_bdd import scenario
from functools import partial
import pytest


scenario = partial(scenario,
                   '../features/singleprovider_space_management.feature')


@pytest.fixture(scope="module", params=["singleprovider_space_management"])
def env_description_file(request):
    return env_file(CUSTOM_CUCUMBER_ENV_DIR, request.param)


@scenario('Create space and don\'t support it')
def test_create_space_no_support(env_description_file):
    pass


@scenario('Create space and support it')
def test_create_space_support(env_description_file):
    pass


@pytest.mark.xfail_env(envs=["singleprovider_space_management"],
                       reason="space owner cannot read what invited user wrote to file")
@scenario('Invite user to unused space')
def test_invite(env_description_file):
    pass


@scenario('Remove user from space')
def test_remove_user(env_description_file):
    pass


@scenario('Delete supported space')
def test_delete_space(env_description_file):
    pass


@scenario('Exceed quota')
def test_exceed_quota(env_description_file):
    pass
