"""Author: Piotr Ociepka
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Example test suite for authorization and mounting onedata client.
"""
from pytest_bdd import scenario

from steps.env_steps import *
from steps.auth_steps import *
from steps.common import *

@scenario(
    '../features/authorization.feature',
    'Successful authorization',
    # example_converters=dict(token=str, spaces=str)
)
def test_successful_authorization():
    pass

@scenario(
    '../features/authorization.feature',
    'Bad authorization'
)
def test_bad_authorization():
    pass
