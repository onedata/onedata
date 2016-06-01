from tests import *
from tests.cucumber.steps.spaces_steps import *
from tests.cucumber.steps.env_steps import *
# from tests.cucumber.steps.auth_steps import *
from tests.cucumber.steps.user_steps import *
from tests.cucumber.steps.multi_file_steps import *
from tests.cucumber.steps.multi_reg_file_steps import *
from tests.cucumber.steps.multi_dir_steps import *

from pytest_bdd import scenario
import pytest


@pytest.fixture(scope="module", params=["auth.json"])
def env_description_file(request):
    return os.path.join(CUSTOM_CUCUMBER_ENV_DIR, request.param)


# @scenario(
#         '../features/space_management.feature',
#         'Check default space'
# )
# def test_default_space(env_description_file):
#     pass


@scenario(
        '../features/space_management.feature',
        'New space with support'
)
def test_support_space(env_description_file):
    pass


# @scenario(
#         '../features/space_management.feature',
#         'New space without support'
# )
# def test_new_space_without_support(env_description_file):
#     pass
#
#
# @scenario(
#         '../features/space_management.feature',
#         'Invite user to default space'
# )
# def test_invite_to_default_space(env_description_file):
#     pass
#
#
# @scenario(
#         '../features/space_management.feature',
#         'Invite user to non-default space'
# )
# def test_invite_to_non_default_space(env_description_file):
#     pass
#
#
# @scenario(
#         '../features/space_management.feature',
#         'Remove user from space'
# )
# def test_remove_user_from_space(env_description_file):
#     pass
#
#
# @scenario(
#         '../features/space_management.feature',
#         'Delete space'
# )
# def test_delete_space(env_description_file):
#     pass
