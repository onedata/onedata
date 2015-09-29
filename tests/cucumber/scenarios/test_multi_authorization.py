"""Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite for authorization and mounting onedata client.
"""
from pytest_bdd import scenario

from steps.env_steps import *
from steps.auth_steps import *
from steps.common import *

@scenario(
    '../features/multi_authorization.feature',
    'Successful authorization',
)
def test_successful_authorization():
    pass

@scenario(
    '../features/multi_authorization.feature',
    'Bad and good authorization',
)
def test_good_and_bad_authorization():
    pass

@scenario(
    '../features/multi_authorization.feature',
    'Bad authorization',
)
def test_bad_authorization():
    pass
