"""Utils and fixtures to facilitate operations on file browser in oneprovider web GUI.
"""

from contextlib import contextmanager

from selenium.webdriver import ActionChains
from selenium.webdriver.common.keys import Keys

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import WebElement, WebElementsSequence, Label, \
    WebItemsSequence
from tests.gui.utils.generic import iter_ahead
from tests.gui.utils.oneprovider.file_browser.file_row import FileRow
from tests.gui.utils.oneprovider.file_browser.metadata_row import MetadataRow

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class FileBrowser(PageObject):
    empty_dir_msg = Label('.empty-model-container')
    files = WebItemsSequence('tbody tr.file-row', cls=FileRow)
    _empty_dir_icon = WebElement('.empty-dir-image')
    _files_with_metadata = WebElementsSequence('tbody tr.first-level')
    _bottom = WebElement('.file-row-load-more')

    def __str__(self):
        return 'file browser in {}'.format(self.parent)

    def is_empty(self):
        try:
            self._empty_dir_icon
        except RuntimeError:
            return False
        else:
            return True

    def get_metadata_for(self, name):
        for item1, item2 in iter_ahead(self._files_with_metadata):
            if 'file-row' in item1.get_attribute('class'):
                if 'file-row' not in item2.get_attribute('class'):
                    if FileRow(self.driver, item1, self).name == name:
                        return MetadataRow(self.driver, item2, self)
        else:
            raise RuntimeError('no metadata row for "{name}" in {item} '
                               'found'.format(name=name, item=self))

    def scroll_to_bottom(self):
        self.driver.execute_script('arguments[0].scrollIntoView();',
                                   self._bottom)

    @contextmanager
    def select_files(self):
        action = ActionChains(self.driver)

        action.shift_down = lambda: action.key_down(Keys.LEFT_SHIFT)
        action.shift_up = lambda: action.key_up(Keys.LEFT_SHIFT)
        action.ctrl_down = lambda: action.key_down(Keys.LEFT_CONTROL)
        action.ctrl_up = lambda: action.key_up(Keys.LEFT_CONTROL)
        action.select = lambda item: action.click(item.web_elem)

        yield action

        action.perform()
