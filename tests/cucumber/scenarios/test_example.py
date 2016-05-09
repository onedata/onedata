"""Author: Piotr Ociepka
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Example test suite for acceptance tests with pytest-bdd.
In features/example.feature file we could define tests using Gherkin language.
This file represents one test suite - we have to declare all scenarios from .feature file.
Tests steps implementation are imported from scenarios/steps directory.
"""
from tests import *
from pytest_bdd import scenario
from tests.cucumber.steps.cucumber_utils import *
from tests.cucumber.steps.env_steps import *

import pytest


@pytest.fixture(scope="module", params=["env.json"])
def env_description_file(request):
    absolute_path = os.path.join(DEFAULT_CUCUMBER_ENV_DIR, request.param)
    return absolute_path


@scenario(
        '../features/example.feature',
        'Hello world'
)
def test_hello(env_description_file):
    """This tag defines which scenario we want to perform.
    test_hello() function could have pass-implementation.
    """
    pass
