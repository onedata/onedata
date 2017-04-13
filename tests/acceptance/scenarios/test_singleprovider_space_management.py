"""Test suite for space management in onedata in singleprovider environment
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.acceptance.steps.spaces_steps import *
from tests.acceptance.steps.env_steps import *
from tests.acceptance.steps.auth_steps import *
from tests.acceptance.steps.multi_auth_steps import *
from tests.acceptance.steps.user_steps import *
from tests.acceptance.steps.multi_file_steps import *
from tests.acceptance.steps.multi_reg_file_steps import *
from tests.acceptance.steps.multi_dir_steps import *
from tests.utils.acceptance_utils import *

from pytest_bdd import scenario
from functools import partial
import pytest


scenario = partial(scenario,
                   '../features/singleprovider_space_management.feature')


@scenario('Create space and don\'t support it')
def test_create_space_no_support(env_description_file):
    pass


@scenario('Create space and support it')
def test_create_space_support(env_description_file):
    pass


@pytest.mark.xfail_env(
    envs=["singleprovider_space_management"],
    reason="Invited user gets 'Protocol error' after space is supported")
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
