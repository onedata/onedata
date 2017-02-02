"""Utils and fixtures to facilitate operations on data tab in oneprovider web GUI.
"""

from tests.gui.utils.generic import find_web_elem, click_on_web_elem
from tests.gui.utils.oneprovider.breadcrumbs import Breadcrumbs
from tests.gui.utils.oneprovider.data_tab.sidebar import DataTabSidebar
from tests.gui.utils.oneprovider.file_browser.browser import FileBrowser

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class DataTab(object):
    def __init__(self, driver, web_elem):
        self.web_elem = web_elem
        self._driver = driver

    @property
    def top_bar(self):
        css_sel = 'header nav.navbar ul.data-files-list-toolbar'
        err_msg = 'unable to locate tool top bar in data tab in op'
        tool_bar = find_web_elem(self._driver, css_sel, err_msg)
        return DataTopToolBar(self._driver, tool_bar)

    @property
    def breadcrumbs(self):
        css_sel = '.secondary-top-bar .file-breadcrumbs-list'
        err_msg = 'unable to locate breadcrumbs in data tab in op'
        breadcrumbs = find_web_elem(self._driver, css_sel, err_msg)
        return Breadcrumbs(self._driver, breadcrumbs)

    @property
    def sidebar(self):
        css_sel = '.lower-main-content nav.secondary-sidebar, ' \
                  '.lower-main-content #data-sidebar-resize-handler'

        items = self.web_elem.find_elements_by_css_selector(css_sel)
        if len(items) != 2:
            raise RuntimeError('unable to locate directory tree sidebar or '
                               'resize handler for it in data tab in op')
        else:
            return DataTabSidebar(self._driver, *items)

    @property
    def file_browser(self):
        css_sel = '.lower-main-content .data-files-list'
        err_msg = 'unable to locate file browser in data tab in op'
        file_browser = find_web_elem(self._driver, css_sel, err_msg)
        return FileBrowser(self._driver, file_browser)


class DataTopToolBar(object):
    def __init__(self, driver, web_elem):
        self.web_elem = web_elem
        self._driver = driver

    def click_on(self, btn_name):
        css_sel = 'a[data-original-title="{:s}"]'.format(btn_name)
        err_msg = 'unable to find "{}" btn in tool top bar in data tab in op'
        btn = find_web_elem(self.web_elem, css_sel, err_msg.format(btn_name))
        err_msg = 'clicking on "{}" btn in tool top bar in data tab in op ' \
                  'disabled'.format(btn_name)
        click_on_web_elem(self._driver, btn, err_msg)
