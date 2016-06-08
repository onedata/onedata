"""Test suite for CRUD operations on directories in onedata.
"""
__author__ = "Jakub Kudzia, Piotr Ociepka"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import pytest
from pytest_bdd import scenario

from tests import *
from tests.utils.path_utils import env_file


@pytest.fixture(scope="module",
                params=["singleprovider_singleclient_directio",
                        "singleprovider_singleclient_proxy"])
def env_description_file(request):
    return env_file(CUSTOM_CUCUMBER_ENV_DIR, request.param)


@scenario(
    '../features/directory_CRUD.feature',
    'Create directory'
)
def test_create(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Create directory in default space'
)
def test_create_default_spaces(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Create directory in non-default space'
)
def test_create_in_spaces(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Create directory spaces'
)
def test_create_spaces_dir(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Create space'
)
def test_create_space(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Rename directory'
)
def test_rename(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Delete empty directory'
)
def test_delete(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Delete directory spaces'
)
def test_delete_spaces_dir(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Delete space'
)
def test_delete_space(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Child directories'
)
def test_children(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Child directories 2'
)
def test_children2(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Duplication'
)
def test_duplication(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Duplication in spaces'
)
def test_duplication_spaces(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Delete empty directory and parents'
)
def test_delete_parents(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Delete non-empty directory in wrong way'
)
def test_delete_non_empty_wrong(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Delete non-empty directory'
)
def test_delete_non_empty(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Move directory'
)
def test_move(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Copy directory'
)
def test_copy(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Move directory to itself'
)
def test_move_to_itself(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Move directory to its subtree'
)
def test_move_to_subtree(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Move directory to itself in spaces'
)
def test_move_to_itself_spaces(env_description_file):
    pass


@pytest.mark.xfail_env(envs=["singleprovider_singleclient_directio",
                             "singleprovider_singleclient_proxy"],
                       reason="Move fails")
@scenario(
    '../features/directory_CRUD.feature',
    'Move directory to itself in default space'
)
def test_move_to_itself_default_space(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Move directory to its subtree in spaces'
)
def test_move_to_subtree_spaces(env_description_file):
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Move directory to its subtree in default space'
)
def test_move_to_subtree_default_space(env_description_file):
    pass
