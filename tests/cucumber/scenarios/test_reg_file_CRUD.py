"""
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite for CRUD operations on regular files in onedata.
"""

from pytest_bdd import scenario

from steps.env_steps import *
from steps.auth_steps import *
from steps.dir_steps import *
from steps.file_steps import *
from steps.reg_file_steps import *
from steps.common import *


@scenario(
    '../features/reg_file_CRUD.feature',
    'Create regular file'
)
def test_create():
    pass


@scenario(
    '../features/reg_file_CRUD.feature',
    'Rename regular file'
)
def test_rename():
    pass


@scenario(
    '../features/reg_file_CRUD.feature',
    'Delete regular file'
)
def test_delete():
    pass


@scenario(
    '../features/reg_file_CRUD.feature',
    'Read and write to regular file'
)
def test_read_write():
    pass


@scenario(
    '../features/reg_file_CRUD.feature',
    'Move regular file and read'
)
def test_move():
    pass


@scenario(
    '../features/reg_file_CRUD.feature',
    'Move big regular file and check MD5'
)
def test_move_big():
    pass


@scenario(
    '../features/reg_file_CRUD.feature',
    'Copy regular file and read'
)
def test_copy():
    pass


@scenario(
    '../features/reg_file_CRUD.feature',
    'Copy big regular file and check MD5'
)
def test_copy_big():
    pass