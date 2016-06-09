from tests import *
from tests.cucumber.steps.spaces_steps import *
from tests.cucumber.steps.env_steps import *
# from tests.cucumber.steps.auth_steps import *
from tests.cucumber.steps.user_steps import *
from tests.cucumber.steps.multi_file_steps import *
from tests.cucumber.steps.multi_reg_file_steps import *
from tests.cucumber.steps.multi_dir_steps import *

from pytest_bdd import scenario
from functools import partial
import pytest

from tests.utils.path_utils import env_file

scenario = partial(scenario,
                   '../features/singleprovider_space_management.feature')


@pytest.fixture(scope="module", params=["singleprovider_space_management.json"])
def env_description_file(request):
    return env_file(CUSTOM_CUCUMBER_ENV_DIR, request.param)


@scenario('Default space without support')
def test_default_space_without_support(env_description_file):
    """CAUTION: this test must be first in the suite because we don't delete
    user's default spaces in teardown so it won't pass after supporting it"""
    pass


@scenario('Default space with support')
def test_default_space(env_description_file):
    pass


@scenario('New space with support')
def test_support_space(env_description_file):
    pass


@scenario('New space without support')
def test_new_space_without_support(env_description_file):
    pass


@scenario('Invite user to default space')
def test_invite_to_default_space(env_description_file):
    pass


@scenario('Invite user to non-default space')
def test_invite_to_non_default_space(env_description_file):
    pass


@scenario('Remove user from space')
def test_remove_user_from_space(env_description_file):
    pass


# TODO
@pytest.mark.xfail_env(envs=["singleprovider_space_management"],
                       reason="Deleting default space returns 403")
@scenario('Delete supported default space')
def test_delete_supported_default_space(env_description_file):
    pass


@scenario('Delete supported non-default space')
def test_delete_supported_non_default_space(env_description_file):
    pass
