"""Author: Michal Wrona
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite for operations on different storages with provider luma
"""
import pytest

from tests.test_common import custom_cucumber_env_dir

from pytest_bdd import scenario

from steps.env_steps import *
from steps.auth_steps import *
from steps.file_steps import *
from steps.reg_file_steps import *


@pytest.fixture(scope="module", params=["env_luma_provider.json"])
def env_description_file(request):
    absolute_path = os.path.join(custom_cucumber_env_dir, request.param)
    return absolute_path


@scenario(
    '../features/luma_provider.feature',
    'Operations on POSIX storage'
)
def test_posix_storage_operations(env_description_file):
    pass


@scenario(
    '../features/luma_provider.feature',
    'Operations on CEPH storage'
)
def test_ceph_storage_operations(env_description_file):
    pass


@scenario(
    '../features/luma_provider.feature',
    'Operations on Amazon S3 storage'
)
def test_s3_storage_operations(env_description_file):
    pass
