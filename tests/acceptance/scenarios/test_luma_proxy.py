"""Test suite for operations on different storages with proxy luma
"""
__author__ = "Michal Wrona"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.acceptance.steps.auth_steps import *
from tests.acceptance.steps.file_steps import *
from tests.acceptance.steps.reg_file_steps import *
from tests.acceptance.steps.env_steps import *

from pytest_bdd import scenario
from functools import partial


scenario = partial(scenario, '../features/luma_proxy.feature')


@scenario('Operations on POSIX storage')
def test_posix_storage_operations(env_description_file):
    pass


@scenario('Operations on CEPH storage')
def test_ceph_storage_operations(env_description_file):
    pass


@scenario('Operations on Amazon S3 storage')
def test_s3_storage_operations(env_description_file):
    pass


@scenario('Operations on Openstack Swift storage')
def test_swift_storage_operations(env_description_file):
    pass    
