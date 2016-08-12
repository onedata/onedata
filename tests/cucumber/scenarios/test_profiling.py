"""Test suite for authorization and mounting onedata client.
"""
__author__ = "Jakub Kudzia, Piotr Ociepka"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests import *
from tests.cucumber.steps.auth_steps import *
from tests.cucumber.steps.multi_auth_steps import *
from tests.utils.cucumber_utils import *
from tests.cucumber.steps.env_steps import *
from tests.cucumber.steps.profiling_steps import *
from tests.utils.path_utils import env_file

from pytest_bdd import scenario
import pytest
from functools import partial


@pytest.fixture(scope="module",
                params=["singleclient_authorization"])
def env_description_file(request):
    return env_file(CUSTOM_CUCUMBER_ENV_DIR, request.param)




scenario = partial(scenario, '../features/profiling.feature')


@scenario('Start oneclient')
def test_start_oneclient(env_description_file):
    pass
