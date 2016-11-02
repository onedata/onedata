"""Example test suite for acceptance tests with pytest-bdd.
Test scenario is defined in tests/acceptance/features/example.feature file in
Gherkin language. This file represents one test suite - we have to declare all
scenarios from .feature file.
Tests steps implementation are imported from tests/acceptance/steps directory.
"""
__author__ = "Jakub Kudzia, Piotr Ociepka"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests import *
from pytest_bdd import scenario
from tests.utils.acceptance_utils import *
from tests.acceptance.steps.env_steps import *
from tests.utils.path_utils import env_file

import pytest


@pytest.fixture(scope="module",
                params=["singleprovider_singleclient_directio"])
def env_description_file(request):
    return env_file(CUSTOM_ACCEPTANCE_ENV_DIR, request.param)


@scenario('../features/example.feature', 'Hello world')
def test_hello(env_description_file):
    """@scenario decorator defines which test will ber run. This function can
    have 'pass' implementation.
    """
    pass
