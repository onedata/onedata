"""Steps directly interacting with local file system.
"""

import yaml
import stat

from pytest_bdd import given, parsers

from tests.gui.utils.generic import suppress


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


PERMS_777 = stat.S_IRWXU | stat.S_IRWXG | stat.S_IROTH | stat.S_IXOTH


@given(parsers.parse('directory tree structure on local '
                     'file system:\n{structure}'))
def create_dir_tree_structure_on_local_fs(structure, tmpdir):
    """Create directory tree structure on local storage.

    Directory tree structure format given in yaml is as follow:

    user_name:                ---> currently we identify user
                                   account with concrete
                                   browser so user_name == browser_id
        - dir1: 5             ---> if item name startswith 'dir' it is
                                   considered directory otherwise a file;
                                   with given num, [num] files with random
                                   contest will be created
        - dir2:
            - dir21
            - dir22:
                - file1.txt: >
                    guana_bana_kunkwa_persi_mona_sala  ---> when specifying file,
                                                            one can specify it's
                                                            content as well
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
                new_dir.chmod(PERMS_777)
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
    file_.chmod(PERMS_777)
