"""
Author: Piotr Ociepka
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite for CRUD operations on directories in onedata.
"""

from pytest_bdd import scenario

from steps.env_steps import *
from steps.auth_steps import *
from steps.dir_steps import *
from steps.file_steps import *
from steps.common import *


@scenario(
    '../features/directory_CRUD.feature',
    'Create directory'
)
def test_create():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Create directory in default space'
)
def test_create_default_spaces():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Create directory in non-default space'
)
def test_create_in_spaces():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Create directory spaces'
)
def test_create_spaces_dir():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Create space'
)
def test_create_space():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Rename directory'
)
def test_rename():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Delete empty directory'
)
def test_delete():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Delete directory spaces'
)
def test_delete_spaces_dir():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Delete space'
)
def test_delete_space():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Child directories'
)
def test_children():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Child directories 2'
)
def test_children2():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Duplication'
)
def test_duplication():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Duplication in spaces'
)
def test_duplication_spaces():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Delete empty directory and parents'
)
def test_delete_parents():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Delete non-empty directory in wrong way'
)
def test_delete_non_empty_wrong():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Delete non-empty directory'
)
def test_delete_non_empty():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Move directory'
)
def test_move():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Copy directory'
)
def test_copy():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Move directory to itself'
)
def test_move_to_itself():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Move directory to its subtree'
)
def test_move_to_subtree():
    pass

# TODO VFS-1504
# @scenario(
#     '../features/directory_CRUD.feature',
#     'Move directory to itself in spaces'
# )
# def test_move_to_itself_spaces():
#     pass
#
#
# @scenario(
#     '../features/directory_CRUD.feature',
#     'Move directory to itself in default space'
# )
# def test_move_to_itself_default_space():
#     pass
#
#
# @scenario(
#     '../features/directory_CRUD.feature',
#     'Move directory to its subtree in spaces'
# )
# def test_move_to_subtree_spaces():
#     pass
#
#
# @scenario(
#     '../features/directory_CRUD.feature',
#     'Move directory to its subtree in default space'
# )
# def test_move_to_subtree_default_space():
#     pass
