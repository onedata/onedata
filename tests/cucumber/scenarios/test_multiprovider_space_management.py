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
                   '../features/multiprovider_space_management.feature')


@pytest.fixture(scope="module", params=["multiprovider_space_management"])
def env_description_file(request):
    return env_file(CUSTOM_CUCUMBER_ENV_DIR, request.param)


@scenario('Default space with support')
def test_default_space(env_description_file):
    pass


@scenario("Default space of other user supported by other provider")
def test_default_space_supported_by_other_provider(env_description_file):
    pass


@scenario("Non-default space of other user supported by other provider")
def test_non_default_space_supported_by_other_provider(env_description_file):
    pass


@scenario("Space supported by two providers")
def test_space_supported_by_2_providers(env_description_file):
    pass


@scenario("Remove user from space")
def test_remove_user(env_description_file):
    pass


@scenario('Delete supported default space')
def test_delete_supported_default_space(env_description_file):
    pass


@scenario('Delete supported non-default space')
def test_delete_supported_non_default_space(env_description_file):
    pass
