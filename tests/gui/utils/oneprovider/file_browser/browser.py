"""Utils and fixtures to facilitate operations on file browser in oneprovider web GUI.
"""

from itertools import izip

from selenium.common.exceptions import NoSuchElementException

from tests.gui.utils.generic import find_web_elem
from tests.gui.utils.oneprovider.file_browser.file_row import FileRow
from tests.gui.utils.oneprovider.file_browser.metadata_row import MetadataRow

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class FileBrowser(object):
    def __init__(self, driver, web_elem):
        self.web_elem = web_elem
        self._driver = driver

    @property
    def is_empty(self):
        css_sel = 'table.files-table'
        try:
            self.web_elem.find_element_by_css_selector(css_sel)
        except NoSuchElementException:
            return True
        else:
            return False

    def __str__(self):
        return 'file browser'

    @property
    def items(self):
        return [FileRow(self._driver, item, self) for item in self._get_items()]

    def __getitem__(self, selector):
        if isinstance(selector, int):
            items_count = self.items_count
            if selector >= items_count:
                raise RuntimeError('requested index {index} out of bound '
                                   '{limit}'.format(index=selector,
                                                    limit=items_count))
            else:
                return FileRow(self._driver, self._get_items()[selector], self)
        elif isinstance(selector, (str, unicode)):
            for item in self.items:
                if item.name == selector:
                    return item
            else:
                raise RuntimeError('unable to find "{name}" in file '
                                   'browser'.format(name=selector))

    @property
    def items_count(self):
        return len(self._get_items())

    def _get_items(self):
        css_sel = 'tbody tr.file-row'
        return self.web_elem.find_elements_by_css_selector(css_sel)

    def get_metadata_for(self, name):
        css_sel = 'tbody tr.first-level'
        items = self.web_elem.find_elements_by_css_selector(css_sel)
        items_ahead = iter(items)
        items_ahead.next()
        for item1, item2 in izip(items, items_ahead):
            if 'file-row' in item1.get_attribute('class'):
                if FileRow(self._driver, item1, self).name == name:
                    if 'file-row' not in item2.get_attribute('class'):
                        return MetadataRow(self._driver, item2)
        else:
            raise RuntimeError('no metadata row for "{name}" in file browser '
                               'found'.format(name=name))

    def scroll_to_bottom(self):
        css_sel = '.file-row-load-more'
        err_msg = 'unable to find bottom of file browser'
        bottom = find_web_elem(self.web_elem, css_sel, err_msg)
        self._driver.execute_script('arguments[0].scrollIntoView();', bottom)
