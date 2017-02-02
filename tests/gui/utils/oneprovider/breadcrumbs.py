"""Utils and fixtures to facilitate operations on breadcrumbs in oneprovider web GUI.
"""

from itertools import izip
from tests.gui.utils.generic import click_on_web_elem


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Breadcrumbs(object):
    def __init__(self, driver, web_elem):
        self.web_elem = web_elem
        self._driver = driver

    def pwd(self):
        return '/'.join(directory.text for directory in self._get_breadcrumbs())

    def chdir(self, path):
        breadcrumbs = self._get_breadcrumbs()
        i, dir1, dir2 = None, None, None
        err_msg = '{} not found on {} position in breadcrumbs, ' \
                  'instead we have {}'
        for i, (dir1, dir2) in enumerate(izip(path.split('/'), breadcrumbs)):
            assert dir1 == dir2.text, err_msg.format(dir1, i, self.pwd())

        err_msg = 'clicking on {}nt element in files breadcrumbs "{}" disabled'
        click_on_web_elem(self._driver, dir2, err_msg.format(i, dir1))

    def _get_breadcrumbs(self):
        css_sel = 'a.file-breadcrumb-item-link'
        return self.web_elem.find_elements_by_css_selector(css_sel)
