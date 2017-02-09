"""Utils and fixtures to facilitate operations on toolbar in data tab in oneprovider web GUI.
"""

from tests.gui.utils.common.common import PageObject
from tests.gui.utils.generic import find_web_elem

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class DataTopToolBar(PageObject):
    def __str__(self):
        return 'toolbar in {}'.format(self._parent)

    def __getitem__(self, item):
        css_sel = 'a[data-original-title="{:s}"]'.format(item)
        btn = find_web_elem(self.web_elem, css_sel,
                            lambda: '"{}" btn in {} not found'.format(item,
                                                                      self))
        return ToolButton(self._driver, btn, self)


class ToolButton(PageObject):
    def __str__(self):
        btn_name = self.web_elem.get_attribute('data-original-title')
        return '{} btn in {}'.format(btn_name, self._parent)

    def is_enabled(self):
        return 'disabled' not in self.web_elem.get_attribute('class')
