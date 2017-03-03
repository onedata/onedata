"""Utils and fixtures to facilitate operations on file browser in oneprovider web GUI.
"""

from contextlib import contextmanager

from selenium.webdriver import ActionChains
from selenium.webdriver.common.keys import Keys

from tests.gui.utils.common.common import PageObject
from tests.gui.utils.common.web_elements import WebElement, ItemListWebElement, IconWebElement, TextLabelWebElement
from tests.gui.utils.generic import iter_ahead, nth
from tests.gui.utils.oneprovider.file_browser.file_row import FileRow
from tests.gui.utils.oneprovider.file_browser.metadata_row import MetadataRow

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class FileBrowser(PageObject):
    empty_dir_msg = TextLabelWebElement('.empty-model-container')
    _empty_dir_icon = IconWebElement('.empty-dir-image')
    _files = ItemListWebElement('tbody tr.file-row')
    _files_with_metadata = ItemListWebElement('tbody tr.first-level')
    _bottom = WebElement('.file-row-load-more')

    def __str__(self):
        return 'file browser in {}'.format(self._parent)

    def __iter__(self):
        return (FileRow(self._driver, item, self) for item in self._files)

    def __getitem__(self, selector):
        if isinstance(selector, int):
            items_count = self.files_count
            if selector >= items_count:
                raise RuntimeError('requested index {index} out of bound '
                                   '{limit}'.format(index=selector,
                                                    limit=items_count))
            else:
                return FileRow(self._driver, nth(self._files, selector), self)
        elif isinstance(selector, (str, unicode)):
            for item in self:
                if item.name == selector:
                    return item
            else:
                raise RuntimeError('unable to find "{name}" in '
                                   '{item}'.format(name=selector, item=self))

    @property
    def files_count(self):
        return len(self._files)

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
                    if FileRow(self._driver, item1, self).name == name:
                        return MetadataRow(self._driver, item2, self)
        else:
            raise RuntimeError('no metadata row for "{name}" in {item} '
                               'found'.format(name=name, item=self))

    def scroll_to_bottom(self):
        self._driver.execute_script('arguments[0].scrollIntoView();',
                                    self._bottom)

    @contextmanager
    def select_files(self):
        action = ActionChains(self._driver)

        action.shift_down = lambda: action.key_down(Keys.LEFT_SHIFT)
        action.shift_up = lambda: action.key_up(Keys.LEFT_SHIFT)
        action.ctrl_down = lambda: action.key_down(Keys.LEFT_CONTROL)
        action.ctrl_up = lambda: action.key_up(Keys.LEFT_CONTROL)
        action.select = lambda item: action.click(item.web_elem)

        yield action

        action.perform()
