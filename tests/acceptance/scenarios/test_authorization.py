"""Test suite for authorization and mounting onedata client.
"""
__author__ = "Jakub Kudzia, Piotr Ociepka"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.acceptance.steps.auth_steps import *
from tests.acceptance.steps.env_steps import *

from pytest_bdd import scenario
from functools import partial

scenario = partial(scenario, '../features/authorization.feature')


@scenario('Successful authorization')
def test_successful_authorization(env_description_file):
    pass


@scenario('Bad authorization')
def test_bad_authorization(env_description_file):
    pass
