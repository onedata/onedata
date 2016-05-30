"""Test suite for operations on different storages with proxy luma
"""
__author__ = "Michal Wrona"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests import *
from tests.utils.path_utils import env_file
from tests.cucumber.steps.auth_steps import *
from tests.cucumber.steps.file_steps import *
from tests.cucumber.steps.reg_file_steps import *
from tests.cucumber.steps.env_steps import *

from pytest_bdd import scenario
import pytest


@pytest.fixture(scope="module", params=["env_luma_proxy"])
def env_description_file(request):
    return env_file(CUSTOM_CUCUMBER_ENV_DIR, request.param)


@scenario(
    '../features/luma_proxy.feature',
    'Operations on POSIX storage'
)
def test_posix_storage_operations(env_description_file):
    pass


@scenario(
    '../features/luma_proxy.feature',
    'Operations on CEPH storage'
)
def test_ceph_storage_operations(env_description_file):
    pass


@scenario(
    '../features/luma_proxy.feature',
    'Operations on Amazon S3 storage'
)
def test_s3_storage_operations(env_description_file):
    pass
