"""Utils and fixtures to facilitate file selection in file browser in oneprovider web GUI.
"""

from contextlib import contextmanager

from selenium.webdriver import ActionChains
from selenium.webdriver.common.keys import Keys

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class FileSelector(object):
    def __init__(self, driver):
        self._driver = driver
        self.action = ActionChains(driver)

    def perform(self):
        self.action.perform()

    def shift_down(self):
        self.action.key_down(Keys.LEFT_SHIFT)

    def shift_up(self):
        self.action.key_up(Keys.LEFT_SHIFT)

    def ctrl_down(self):
        self.action.key_down(Keys.LEFT_CONTROL)

    def ctrl_up(self):
        self.action.key_up(Keys.LEFT_CONTROL)

    def select(self, item):
        if not item.is_selected:
            self.action.click(item.web_elem)
        else:
            raise RuntimeError('file "{}" in file browser is already '
                               'selected'.format(item.name))


@contextmanager
def select_files(driver):
    action = FileSelector(driver)
    yield action
    action.perform()
