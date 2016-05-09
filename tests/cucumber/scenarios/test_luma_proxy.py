"""Author: Michal Wrona
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite for operations on different storages with proxy luma
"""
from tests import *
from tests.cucumber.steps.auth_steps import *
from tests.cucumber.steps.file_steps import *
from tests.cucumber.steps.reg_file_steps import *
from tests.cucumber.steps.env_steps import *

from pytest_bdd import scenario
import pytest


@pytest.fixture(scope="module", params=["env_luma_proxy.json"])
def env_description_file(request):
    absolute_path = os.path.join(CUSTOM_CUCUMBER_ENV_DIR, request.param)
    return absolute_path


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
