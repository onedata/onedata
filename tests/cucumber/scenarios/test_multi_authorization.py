"""Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite for authorization and mounting onedata client,
 in multi-client environment.
"""
from pytest_bdd import scenario

from steps.env_steps import *
from steps.auth_steps import *
from steps.common import *


@scenario(
    '../features/multi_authorization.feature',
    'Successful authorization',
)
def test_successful_authorization(env_description_file):
    pass


@scenario(
    '../features/multi_authorization.feature',
    'Successful authorization - 1 client per user',
)
def test_successful_authorization2(env_description_file):
    pass


@scenario(
    '../features/multi_authorization.feature',
    'Successful authorization - 2 clients of one user',
)
def test_successful_authorization3(env_description_file):
    pass


@scenario(
    '../features/multi_authorization.feature',
    'Bad and good authorization',
)
def test_good_and_bad_authorization(env_description_file):
    pass


@scenario(
    '../features/multi_authorization.feature',
    'Bad authorization',
)
def test_bad_authorization(env_description_file):
    pass
