"""Author: Piotr Ociepka
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite for authorization and mounting onedata client.
"""
from tests import *
from tests.cucumber.steps.auth_steps import *
from tests.cucumber.steps.multi_auth_steps import *
from tests.cucumber.steps.cucumber_utils import *
from tests.cucumber.steps.env_steps import *

from pytest_bdd import scenario


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
