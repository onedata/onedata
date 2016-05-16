from tests import *
from tests.cucumber.steps.user_management_steps import *

from pytest_bdd import scenario
import pytest


@pytest.fixture(scope="module", params=["tmp.json"])
def env_description_file(request):
    return os.path.join(CUSTOM_CUCUMBER_ENV_DIR, request.param)


@scenario(
        '../features/user_management.feature',
        'User registration'
)
def test_type_empty(env_description_file):
    pass
