"""
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite for CRUD operations on regular files in onedata.
"""
from tests.cucumber.steps.auth_steps import *
from tests.cucumber.steps.cucumber_utils import *
from tests.cucumber.steps.dir_steps import *
from tests.cucumber.steps.file_steps import *
from tests.cucumber.steps.reg_file_steps import *

from pytest_bdd import scenario
import pytest

@scenario(
    '../features/reg_file_CRUD.feature',
    'Create regular file'
)
def test_create(env_description_file):
    pass


@scenario(
    '../features/reg_file_CRUD.feature',
    'Rename regular file'
)
def test_rename(env_description_file):
    pass


@scenario(
    '../features/reg_file_CRUD.feature',
    'Delete regular file'
)
def test_delete(env_description_file):
    pass


@scenario(
    '../features/reg_file_CRUD.feature',
    'Read and write to regular file'
)
def test_read_write(env_description_file):
    pass


@scenario(
    '../features/reg_file_CRUD.feature',
    'Append regular file'
)
def test_append(env_description_file):
    pass


@scenario(
    '../features/reg_file_CRUD.feature',
    'Replace word in file'
)
def test_replace(env_description_file):
    pass


@scenario(
    '../features/reg_file_CRUD.feature',
    'Move regular file and read'
)
def test_move(env_description_file):
    pass


# TODO VFS-2005
@pytest.mark.xfail_env(
        envs=["env", "env2", "env3"],
        reason="reading fails in oneclient")
@scenario(
    '../features/reg_file_CRUD.feature',
    'Move big regular file and check MD5'
)
def test_move_big(env_description_file):
    pass


@scenario(
    '../features/reg_file_CRUD.feature',
    'Copy regular file and read'
)
def test_copy(env_description_file):
    pass


# TODO VFS-2005
@pytest.mark.xfail_env(
        envs=["env", "env2", "env3"],
        reason="reading fails in oneclient")
@scenario(
    '../features/reg_file_CRUD.feature',
    'Copy big regular file and check MD5'
)
def test_copy_big(env_description_file):
    pass
