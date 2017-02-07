"""Utils and fixtures to facilitate operations on breadcrumbs in oneprovider web GUI.
"""

from itertools import izip

from tests.gui.utils.common.common import PageObject
from tests.gui.utils.generic import click_on_web_elem


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Breadcrumbs(PageObject):

    def __str__(self):
        return 'Breadcrumbs({path}) in {parent}'.format(path=self.pwd(),
                                                        parent=str(self._parent))

    def pwd(self):
        return '/'.join(directory.text
                        for directory in self._get_breadcrumbs())

    def chdir(self, path):
        breadcrumbs = self._get_breadcrumbs()
        i, dir1, dir2 = None, None, None
        err_msg = '{dir} not found on {idx}th position in {item}'
        for i, (dir1, dir2) in enumerate(izip(path.split('/'), breadcrumbs)):
            assert dir1 == dir2.text, err_msg.format(dir=dir1, idx=i,
                                                     item=str(self))

        err_msg = 'clicking on {}th element in {} disabled'
        click_on_web_elem(self._driver, dir2, err_msg.format(i, str(self)))

    def _get_breadcrumbs(self):
        css_sel = 'a.file-breadcrumb-item-link'
        return self.web_elem.find_elements_by_css_selector(css_sel)
