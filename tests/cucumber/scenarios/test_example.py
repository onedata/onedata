"""Author: Piotr Ociepka
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Example test suite for acceptance tests with pytest-bdd.
In features/example.feature file we could define tests using Gherkin language.
This file represents one test suite - we have to declare all scenarios from .feature file.
Tests steps implementation are imported from scenarios/steps directory.
"""
from pytest_bdd import scenario

from steps.env_steps import *
from steps.common import *

@scenario(
    '../features/example.feature',
    'Hello world'
    # example_converters=dict(env_json=str, number=int)
)
def test_hello():
    """This tag defines which scenario we want to perform.
    test_hello() function could have pass-implementation.
    """
    pass
