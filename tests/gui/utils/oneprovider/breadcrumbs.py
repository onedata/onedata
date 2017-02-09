"""Utils and fixtures to facilitate operations on breadcrumbs in oneprovider web GUI.
"""

from itertools import izip

from tests.gui.utils.common.common import PageObject
from tests.gui.utils.common.web_elements import ItemListWebElement
from tests.gui.utils.generic import click_on_web_elem


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Breadcrumbs(PageObject):
    _breadcrumbs = ItemListWebElement('a.file-breadcrumb-item-link')

    def __str__(self):
        return 'Breadcrumbs({path}) in {parent}'.format(path=self.pwd(),
                                                        parent=self._parent)

    def pwd(self):
        return '/'.join(directory.text
                        for directory in self._breadcrumbs)

    def chdir(self, path):
        breadcrumbs = self._breadcrumbs
        i, dir1, dir2 = None, None, None
        err_msg = '{dir} not found on {idx}th position in {item}'

        for i, (dir1, dir2) in enumerate(izip(path.split('/'), breadcrumbs)):
            assert dir1 == dir2.text, err_msg.format(dir=dir1, idx=i, item=self)

        click_on_web_elem(self._driver, dir2,
                          lambda: 'cannot click on {idx}th element in '
                                  '{item}'.format(idx=i, item=self))
