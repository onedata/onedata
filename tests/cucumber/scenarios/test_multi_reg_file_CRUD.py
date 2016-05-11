"""
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite for CRUD operations on regular files in onedata,
in multi-client environment.
"""
from tests.cucumber.steps.cucumber_utils import *
from tests.cucumber.steps.multi_auth_steps import *
from tests.cucumber.steps.multi_dir_steps import *
from tests.cucumber.steps.multi_file_steps import *
from tests.cucumber.steps.multi_reg_file_steps import *

from pytest_bdd import scenario
import pytest


@scenario(
    '../features/multi_reg_file_CRUD.feature',
    'Create regular file'
)
def test_create(env_description_file):
    pass


@scenario(
    '../features/multi_reg_file_CRUD.feature',
    'Rename regular file without permission'
)
def test_rename_without_permission(env_description_file):
    pass


@scenario(
    '../features/multi_reg_file_CRUD.feature',
    'Rename regular file with permission'
)
def test_rename_with_permission(env_description_file):
    pass


@scenario(
    '../features/multi_reg_file_CRUD.feature',
    'Delete regular file by owner'
)
def test_delete_by_owner(env_description_file):
    pass


@scenario(
        '../features/multi_reg_file_CRUD.feature',
        'Delete regular file by other user'
)
def test_delete_by_other_user(env_description_file):
    pass


@scenario(
        '../features/multi_reg_file_CRUD.feature',
        'Read and write to regular file'
)
def test_read_write(env_description_file):
    pass


@scenario(
        '../features/multi_reg_file_CRUD.feature',
        'Read regular file without read permission'
)
def test_read_without_permission(env_description_file):
    pass


@scenario(
        '../features/multi_reg_file_CRUD.feature',
        'Write to regular file with write permission'
)
def test_write_with_permission(env_description_file):
    pass


@scenario(
        '../features/multi_reg_file_CRUD.feature',
        'Write to regular file without write permission'
)
def test_write_without_permission(env_description_file):
    pass


@scenario(
        '../features/multi_reg_file_CRUD.feature',
        'Execute file with execute permission'
)
def test_execute_with_permission(env_description_file):
    pass


@scenario(
        '../features/multi_reg_file_CRUD.feature',
        'Execute file without execute permission'
)
def test_execute_without_permission(env_description_file):
    pass


#  TODO
@pytest.mark.xfail_env(
        envs=["env", "env2", "env3"],
        reason="move is being reimplemented")
@scenario(
        '../features/multi_reg_file_CRUD.feature',
        'Move regular file and read'
)
def test_move(env_description_file):
    pass


# TODO
@pytest.mark.xfail_env(
        envs=["env", "env2", "env3"],
        reason="move is being reimplemented")
@scenario(
    '../features/multi_reg_file_CRUD.feature',
    'Move big regular file and check MD5'
)
def test_move_big(env_description_file):
    pass


@scenario(
        '../features/multi_reg_file_CRUD.feature',
        'Copy regular file and read'
)
def test_copy(env_description_file):
    pass


@scenario(
        '../features/multi_reg_file_CRUD.feature',
        'Copy big regular file and check MD5'
)
def test_copy_big(env_description_file):
    pass
