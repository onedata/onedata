"""Utils and fixtures to facilitate operations on file uploader in oneprovider web GUI.
"""

import re

from tests.gui.utils.common.common import PageObject
from tests.gui.utils.common.web_elements import ItemListWebElement, TextLabelWebElement, WebElement
from tests.gui.utils.generic import nth
from tests.gui.utils.oneprovider.file_browser.file_row import FileRow

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class FileUploader(PageObject):
    heading = TextLabelWebElement('.panel-heading')
    _progress = WebElement('.resumable-progress')
    _rows = ItemListWebElement('ul.resumable-list li')

    @property
    def progress(self):
        return ProgressBar(self.driver, self._progress, self)

    def is_visible(self):
        return 'visible' in self.web_elem.get_attribute('class')

    def __str__(self):
        return 'file uploader in {}'.format(self.parent)

    def __iter__(self):
        return (FileUploadRow(self.driver, item, self) for item in self._rows)

    def __getitem__(self, selector):
        if isinstance(selector, int):
            items_count = self.items_count
            if selector >= items_count:
                raise RuntimeError('requested index {index} out of bound '
                                   '{limit}'.format(index=selector,
                                                    limit=items_count))
            else:
                return FileRow(self.driver, nth(self._rows, selector), self)

        elif isinstance(selector, (str, unicode)):
            for item in self:
                if item.name == selector:
                    return item
            else:
                raise RuntimeError('unable to find "{name}" in '
                                   '{item}'.format(name=selector, item=self))

    @property
    def items_count(self):
        return len(self._rows)

    def scroll_to_bottom(self):
        self.driver.execute_script('arguments[0].scrollIntoView();',
                                   self._rows[-1])


class FileUploadRow(PageObject):
    name = TextLabelWebElement('.resumable-file-name')
    progress = TextLabelWebElement('.resumable-file-progress')


class ProgressBar(PageObject):
    progress_text = TextLabelWebElement('.progress-text')
    _progress_container = WebElement('.progress-bar')

    @property
    def progress_bar(self):
        style = self._progress_container.get_attribute('style')
        return re.search(r'width:\s*(\d+?%)', style).group(1)
