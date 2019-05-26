"""Test suite for extended attributes support.
"""
__author__ = "Bartek Kryza"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.acceptance.steps.auth_steps import *
from tests.acceptance.steps.env_steps import *
from tests.utils.acceptance_utils import *
from tests.acceptance.steps.dir_steps import *
from tests.acceptance.steps.file_steps import *
from tests.acceptance.steps.reg_file_steps import *
from tests.acceptance.steps.extended_attributes_steps import *

from pytest_bdd import scenario
from functools import partial


scenario = partial(scenario, '../features/extended_attributes.feature')

@scenario('Check extended attribute exists')
def test_extended_attribute_exists(env_description_file):
    pass

@scenario('Check string extended attribute has correct value')
def test_string_extended_attribute_has_value(env_description_file):
    pass

@scenario('Check JSON extended attribute has correct value')
def test_json_extended_attribute_has_value(env_description_file):
    pass

@scenario('Check numeric extended attribute has correct value')
def test_numeric_extended_attribute_has_value(env_description_file):
    pass
