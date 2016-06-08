"""Test suite for authorization and mounting onedata client.
"""
__author__ = "Jakub Kudzia, Piotr Ociepka"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import pytest
from pytest_bdd import scenario

from tests import *
from tests.utils.path_utils import env_file


@pytest.fixture(scope="module",
                params=["singleclient_authorization"])
def env_description_file(request):
    return env_file(CUSTOM_CUCUMBER_ENV_DIR, request.param)


@scenario(
    '../features/authorization.feature',
    'Successful authorization'
)
def test_successful_authorization(env_description_file):
    pass


@scenario(
    '../features/authorization.feature',
    'Bad authorization'
)
def test_bad_authorization(env_description_file):
    pass
