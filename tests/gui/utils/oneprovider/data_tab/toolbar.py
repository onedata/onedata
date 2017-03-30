"""Utils and fixtures to facilitate operations on toolbar in data tab in oneprovider web GUI.
"""

from tests.gui.utils.common.common import PageObject
from tests.gui.utils.common.web_elements import WebElement
from tests.gui.utils.generic import find_web_elem, rm_css_cls

__author__ = "Jakub Liput, Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class DataTopToolBar(PageObject):
    _upload_input = WebElement('input#toolbar-file-browse')

    def upload_files(self, files):
        """This interaction is very hacky, because uploading files with Selenium
        needs to use input element, but we do not use it directly in frontend.
        So we unhide an input element for a while and pass a local file path to it.
        """
        with rm_css_cls(self.driver, self._upload_input, 'hidden') as elem:
            elem.send_keys(files)

    def __str__(self):
        return 'toolbar in {}'.format(self.parent)

    def __getitem__(self, item):
        css_sel = 'a[data-original-title="{:s}"]'.format(item)
        btn = find_web_elem(self.web_elem, css_sel,
                            lambda: '"{}" btn in {} not found'.format(item,
                                                                      self))
        return ToolButton(self.driver, btn, self)


class ToolButton(PageObject):
    def __str__(self):
        btn_name = self.web_elem.get_attribute('data-original-title')
        return '{} btn in {}'.format(btn_name, self.parent)

    def is_enabled(self):
        return 'disabled' not in self.web_elem.get_attribute('class')
