"""Test suite for CRUD operations on directories in onedata,
in multi-client environment.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import pytest
from pytest_bdd import scenario


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Create directory'
)
def test_create(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Rename someone\'s directory without permission'
)
def test_rename_someone_without_permission(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Rename someone\'s directory with permission'
)
def test_rename_someone_with_permission(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Rename own directory'
)
def test_rename_own(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Delete someone\'s empty directory'
)
def test_delete_someone(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Delete own empty directory'
)
def test_delete_own(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'List directory without read permission'
)
def test_list_dir_without_permission(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Create file in directory without write permission'
)
def test_create_subfile_without_permission(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Create file in directory with write permission'
)
def test_create_subfile_with_permission(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Delete file in directory without write permission'
)
def test_delete_subfile_without_permission(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Delete file in directory with write permission'
)
def test_delete_subfile_with_permission(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Rename file in directory without write permission'
)
def test_rename_subfile_without_permission(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Rename file in directory without write permission'
)
def test_rename_subfile_with_permission(env_description_file):
    pass


# TODO VFS-1824
@pytest.mark.xfail_env(
    envs=["singleprovider_multiclient_directio",
          "singleprovider_multiclient_proxy",
          "multiprovider_proxy",
          "multiprovider_directio"],
    reason="u2 is unable to create direcory with the same name "
           "although first one was deleted")
@scenario(
    '../features/multi_directory_CRUD.feature',
    'Recreate directory deleted by other user'
)
def test_recreate(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Child directories'
)
def test_children(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Child directories 2'
)
def test_children2(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Duplication'
)
def test_duplication(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Delete empty directory and parents'
)
def test_delete_parents(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Delete non-empty directory in wrong way'
)
def test_delete_non_empty_wrong(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Delete non-empty directory'
)
def test_delete_non_empty(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Move directory'
)
def test_move(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Copy directory'
)
def test_copy(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Move directory to itself'
)
def test_move_to_itself(env_description_file):
    pass


@scenario(
    '../features/multi_directory_CRUD.feature',
    'Move directory to its subtree'
)
def test_move_to_subtree(env_description_file):
    pass
