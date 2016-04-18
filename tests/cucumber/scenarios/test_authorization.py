"""Author: Piotr Ociepka
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite for authorization and mounting onedata client.
"""
from tests.test_common import cucumber_env_dir, env_description_files
from pytest_bdd import scenario

from steps.env_steps import *
from steps.auth_steps import *
from steps.common import *


@pytest.fixture(scope="module",
                params=env_description_files(cucumber_env_dir, "env.json",
                                             "env2.json", "env3.json"))
def env_description_file(request):
    return request.param


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
