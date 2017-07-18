"""Utils and fixtures to facilitate operations on file uploader in oneprovider web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


import re

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import (Label, WebElement, WebItem,
                                               WebItemsSequence)


class FileUploadRow(PageObject):
    name = Label('.resumable-file-name')
    progress = Label('.resumable-file-progress')


class ProgressBar(PageObject):
    progress_text = Label('.progress-text')
    _progress_container = WebElement('.progress-bar')

    @property
    def progress_bar(self):
        style = self._progress_container.get_attribute('style')
        return re.search(r'width:\s*(\d+?%)', style).group(1)


class FileUploader(PageObject):
    heading = Label('.panel-heading')
    progress = WebItem('.resumable-progress', cls=ProgressBar)
    rows = WebItemsSequence('ul.resumable-list li', cls=FileUploadRow)

    def is_visible(self):
        return 'visible' in self.web_elem.get_attribute('class')

    def __str__(self):
        return 'file uploader in {}'.format(self.parent)

    def scroll_to_bottom(self):
        self.driver.execute_script('arguments[0].scrollIntoView();',
                                   self.rows[-1].web_elem)
