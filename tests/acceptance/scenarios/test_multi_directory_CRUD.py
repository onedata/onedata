"""Test suite for CRUD operations on directories in onedata,
in multi-client environment.
"""
from tests import DEFAULT_ACCEPTANCE_ENV_DIR
from tests.utils.path_utils import env_file

__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
from tests.acceptance.steps.env_steps import *
from tests.utils.acceptance_utils import *
from tests.acceptance.steps.multi_auth_steps import *
from tests.acceptance.steps.multi_dir_steps import *
from tests.acceptance.steps.multi_file_steps import *

from pytest_bdd import scenario
import pytest
from functools import partial


scenario = partial(scenario, '../features/multi_directory_CRUD.feature')


@scenario('Create directory')
def test_create(env_description_file):
    pass


@scenario('Rename someone\'s directory without permission')
def test_rename_someone_without_permission(env_description_file):
    pass


@scenario('Rename someone\'s directory with permission')
def test_rename_someone_with_permission(env_description_file):
    pass


@scenario('Rename own directory')
def test_rename_own(env_description_file):
    pass


@scenario('Delete someone\'s empty directory')
def test_delete_someone(env_description_file):
    pass


@scenario('Delete own empty directory')
def test_delete_own(env_description_file):
    pass


@scenario('List directory without read permission')
def test_list_dir_without_permission(env_description_file):
    pass


@scenario('Fail to create file in directory without write permission')
def test_fail_to_create_subfile_without_permission(env_description_file):
    pass


@scenario('Delete directory right after deleting its subdirectory by other client')
def test_delete_dir_right_after_deleting_subdir(env_description_file):
    pass


@scenario('Create file in directory with write permission')
def test_create_subfile_with_permission(env_description_file):
    pass


@scenario('Fail to delete file in directory without write permission')
def test_fail_to_delete_subfile_without_permission(env_description_file):
    pass


@scenario('Delete file in directory with write permission')
def test_delete_subfile_with_permission(env_description_file):
    pass


@scenario('Fail to rename file in directory without write permission')
def test_fail_to_rename_subfile_without_permission(env_description_file):
    pass


@scenario('Rename file in directory with write permission')
def test_rename_subfile_with_permission(env_description_file):
    pass


# # TODO VFS-1824
@pytest.mark.xfail_env(
    envs=["singleprovider_multiclient_directio",
          "singleprovider_multiclient_proxy",
          "multiprovider_proxy",
          "multiprovider_directio"],
    reason="u2 is unable to create direcory with the same name "
           "although first one was deleted")
@scenario('Recreate directory deleted by other user')
def test_recreate(env_description_file):
    pass


@scenario('Child directories')
def test_children(env_description_file):
    pass


@scenario('Child directories 2')
def test_children2(env_description_file):
    pass


@scenario('Duplication')
def test_duplication(env_description_file):
    pass


@pytest.mark.xfail_env(
    envs=["multiprovider_proxy",
          "multiprovider_directio"],
    reason="Fails only for multiprovider tests")
@scenario('Delete empty directory and parents')
def test_delete_parents(env_description_file):
    pass


@scenario('Delete non-empty directory in wrong way')
def test_delete_non_empty_wrong(env_description_file):
    pass


@scenario('Delete non-empty directory')
def test_delete_non_empty(env_description_file):
    pass


@scenario('Move directory')
def test_move(env_description_file):
    pass


@scenario('Copy directory')
def test_copy(env_description_file):
    pass


@scenario('Move directory to itself')
def test_move_to_itself(env_description_file):
    pass


@scenario('Move directory to its subtree')
def test_move_to_subtree(env_description_file):
    pass
