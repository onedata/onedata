"""Test suite for authorization and mounting onedata client.
"""
__author__ = "Jakub Kudzia, Piotr Ociepka"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.acceptance.steps.auth_steps import *
from tests.acceptance.steps.multi_auth_steps import *
from tests.utils.acceptance_utils import *
from tests.acceptance.steps.env_steps import *
from tests.acceptance.steps.profiling_steps import *
from tests.acceptance.steps.dir_steps import *
from tests.acceptance.steps.file_steps import *
from tests.acceptance.steps.reg_file_steps import *

from pytest_bdd import scenario
from functools import partial


scenario = partial(scenario, 'profiling.feature')


@scenario('Profile ls')
def test_profile_ls(env_description_file):
    pass


@scenario('Profile ls without remounting')
def test_profile_ls_no_remounting(env_description_file):
    pass


@scenario('Profile ls nested')
def test_profile_ls_nested(env_description_file):
    pass


@scenario('Profile ls nested without remounting')
def test_profile_ls_nested_no_remounting(env_description_file):
    pass


@scenario('Profile mkdir')
def test_profile_mkdir(env_description_file):
    pass


@scenario('Profile rm')
def test_profile_rm(env_description_file):
    pass


@scenario('Profile touch')
def test_profile_touch(env_description_file):
    pass


@scenario('Profile write sysbench')
def test_profile_write_sysbench(env_description_file):
    pass


@scenario('Profile dd')
def test_profile_dd(env_description_file):
    pass


@scenario('Profile write')
def test_profile_write(env_description_file):
    pass


@scenario('Profile read')
def test_profile_read(env_description_file):
    pass
