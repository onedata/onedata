"""Test suite for authorization and mounting onedata client,
 in multi-client environment.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
from tests import *
from tests.cucumber.steps.env_steps import *
from tests.cucumber.steps.cucumber_utils import *
from tests.cucumber.steps.multi_auth_steps import *
from tests.utils.path_utils import env_file

from pytest_bdd import scenario
import pytest
from functools import partial


scenario = partial(scenario, '../features/multi_authorization.feature')


@pytest.fixture(scope="module", params=["multiclient_authorization"])
def env_description_file(request):
    return env_file(CUSTOM_CUCUMBER_ENV_DIR, request.param)


@scenario('Successful authorization - 1 client per user',)
def test_successful_authorization1(env_description_file):
    pass


@scenario('Successful authorization - 2 clients of one user',)
def test_successful_authorization2(env_description_file):
    pass


@scenario('Successful authorization - 2 clients of one user on different hosts',)
def test_successful_authorization3(env_description_file):
    pass


@scenario('Bad and good authorization',)
def test_good_and_bad_authorization(env_description_file):
    pass


@scenario('Bad authorization',)
def test_bad_authorization(env_description_file):
    pass
