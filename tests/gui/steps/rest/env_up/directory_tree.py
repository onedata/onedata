"""Steps for directory tree creation in providers using REST API.
"""

import yaml

from tests.gui.utils.onezone_client import SpaceCreateRequest, SpacePrivileges
from tests.gui.utils.onepanel_client import SpaceSupportRequest
from tests.gui.utils.cdmi_client import ContainerApi

from pytest_bdd import given, parsers

from ..common import get_oz_user_api, get_oz_space_api, get_op_panel_api


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


def _get_storage_id_with_given_name(storage_name, rest_client):
    for storage_id in rest_client.get_storages().ids:
        storage = rest_client.get_storage_details(storage_id)
        if storage.name == storage_name:
            return storage.id


def _get_id_of_users_space_with_given_name(space_name, rest_client):
    for space_id in rest_client.list_user_spaces().spaces:
        space = rest_client.get_user_space(space_id)
        if space.name == space_name:
            return space.space_id


# @given(parsers.parse('initial spaces configuration in "{service}" '
#                      'Onezone service:\n{config}'))


import yaml

from pytest_bdd import given, parsers

from tests.gui.utils.generic import suppress


@given(parsers.parse('directory tree structure on local '
                     'file system:\n{structure}'))
def create_dir_tree_structure_on_local_fs(structure, tmpdir):
    """
        space1:
            options:
                default provider: p1
                random file prefix: file
            directory tree:
                - dir0: []
                - dir1: [1..10]
                - dir2:
                    - file0: huhuszki
                    - file1:
                        provider: p2
                        content: heheszki
                    - file2
    """

    for user, home_dir_content in yaml.load(structure).items():
        home_dir = tmpdir.join(user)
        with suppress(OSError):
            home_dir.mkdir()
        _mkdirs(home_dir, home_dir_content)


def _mkdirs(cwd, dir_content=None):
    if not dir_content:
        return

    try:
        files_num = int(dir_content)
    except TypeError:
        for item in dir_content:
            try:
                [(name, content)] = item.items()
            except AttributeError:
                name = item
                content = None

            if name.startswith('dir'):
                new_dir = cwd.join(name)
                new_dir.mkdir()
                _mkdirs(new_dir, content)
            else:
                _mkfile(cwd.join(name), content)
    else:
        for i in xrange(files_num):
            _mkfile(cwd.join('file{}.txt'.format(i)))


def _mkfile(file_, file_content=None):
    if not file_content:
        file_content = '1' * 10

    file_.write(file_content)
