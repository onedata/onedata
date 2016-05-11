import pytest


@pytest.fixture(autouse=True)
def skip_by_env(skip_by_env):
    pass


@pytest.fixture(autouse=True)
def xfail_by_env(xfail_by_env):
    pass