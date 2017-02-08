"""Utils and fixtures to facilitate operation on file row in file browser
in oneprovider web GUI.
"""

from decorator import contextmanager
from selenium.webdriver import ActionChains
from selenium.webdriver.common.keys import Keys

from tests.gui.utils.common.mixins import ClickableMixin
from tests.gui.utils.generic import click_on_web_elem
from tests.gui.utils.common.common import PageObject
from tests.gui.utils.common.web_elements import TextLabelWebElement, \
    IconWebElement

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class FileRow(PageObject, ClickableMixin):
    name = TextLabelWebElement('.file-label', parent_name='given file row')
    size = TextLabelWebElement('.file-list-col-size')
    modification_date = TextLabelWebElement('.file-list-col-modification')

    _icon = IconWebElement('.file-icon .one-icon')
    _metadata_icon = IconWebElement('.file-row-tools .oneicon-metadata')
    _share_icon = IconWebElement('.file-row-tools .oneicon-share')

    def __str__(self):
        return '{item} in {parent}'.format(item=self.name,
                                           parent=str(self._parent))

    def is_selected(self):
        return 'active' in self.web_elem.get_attribute('class')

    def is_file(self):
        return 'file' in self._icon.get_attribute('class')

    def is_directory(self):
        return 'folder' in self._icon.get_attribute('class')

    def is_shared(self):
        return 'share' in self._icon.get_attribute('class')

    def is_tool_visible(self, name):
        tool = getattr(self, '_{tool}_icon'.format(tool=name))

    def click_on_tool(self, name):
        tool = getattr(self, '_{tool}_icon'.format(tool=name))
        err_msg = 'clicking on "{}" in {} disabled'.format(name, str(self))
        click_on_web_elem(self._driver, tool, err_msg)


@contextmanager
def select_files(driver):
    action = ActionChains(driver)

    action.shift_down = lambda: action.key_down(Keys.LEFT_SHIFT)
    action.shift_up = lambda: action.key_up(Keys.LEFT_SHIFT)
    action.ctrl_down = lambda: action.key_down(Keys.LEFT_CONTROL)
    action.ctrl_up = lambda: action.key_up(Keys.LEFT_CONTROL)
    action.select = lambda item: action.click(item.web_elem)

    yield action

    action.perform()
