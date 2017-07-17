"""Utils for data tab in Oneprovider GUI tests
"""

from tests.gui.utils.core.common import PageObject
from tests.gui.utils.core.web_elements import ItemListWebElement, WebItem
from .file_uploader import FileUploader
from .sidebar import DataTabSidebar
from .toolbar import DataTopToolBar
from ..breadcrumbs import Breadcrumbs
from ..file_browser.browser import FileBrowser

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class DataTab(PageObject):
    toolbar = WebItem('header nav.navbar ul.data-files-list-toolbar',
                      cls=DataTopToolBar)
    breadcrumbs = WebItem('.secondary-top-bar .file-breadcrumbs-list',
                          cls=Breadcrumbs)
    file_browser = WebItem('.lower-main-content .data-files-list',
                           cls=FileBrowser)
    file_uploader = WebItem('#main-content + .file-upload .file-upload',
                            cls=FileUploader)
    _sidebar = ItemListWebElement('.lower-main-content nav.secondary-sidebar, '
                                  '#data-sidebar-resize-handler')

    def __str__(self):
        return 'DATA tab in {}'.format(self.parent)

    @property
    def sidebar(self):
        sidebar, resize_handler = self._sidebar
        return DataTabSidebar(self.web_elem, sidebar, self,
                              resize_handler=resize_handler)
