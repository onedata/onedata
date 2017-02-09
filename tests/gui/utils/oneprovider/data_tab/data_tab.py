"""Utils and fixtures to facilitate operations on data tab in oneprovider web GUI.
"""

from tests.gui.utils.common.common import PageObject
from tests.gui.utils.common.web_elements import WebElement, ItemListWebElement
from tests.gui.utils.oneprovider.breadcrumbs import Breadcrumbs
from tests.gui.utils.oneprovider.data_tab.sidebar import DataTabSidebar
from tests.gui.utils.oneprovider.data_tab.toolbar import DataTopToolBar
from tests.gui.utils.oneprovider.file_browser.browser import FileBrowser

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class DataTab(PageObject):
    _toolbar = WebElement('header nav.navbar ul.data-files-list-toolbar')
    _breadcrumbs = WebElement('.secondary-top-bar .file-breadcrumbs-list')
    _file_browser = WebElement('.lower-main-content .data-files-list')
    _sidebar = ItemListWebElement('.lower-main-content nav.secondary-sidebar, '
                                  '#data-sidebar-resize-handler')

    def __str__(self):
        return 'DATA tab in {}'.format(self._parent)

    @property
    def toolbar(self):
        return DataTopToolBar(self._driver, self._toolbar, self)

    @property
    def breadcrumbs(self):
        return Breadcrumbs(self._driver, self._breadcrumbs, self)

    @property
    def sidebar(self):
        sidebar, resize_handler = self._sidebar
        return DataTabSidebar(self.web_elem, sidebar, self,
                              resize_handler=resize_handler)

    @property
    def file_browser(self):
        return FileBrowser(self._driver, self._file_browser, self)
