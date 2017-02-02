"""Utils and fixtures to facilitate operation on file row in file browser in oneprovider web GUI.
"""

from selenium.webdriver import ActionChains

from tests.gui.utils.generic import find_web_elem, click_on_web_elem

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class FileRow(object):
    def __init__(self, driver, web_elem):
        self.web_elem = web_elem
        self._driver = driver

    @property
    def name(self):
        css_sel = '.file-label'
        err_msg = 'unable to locate name label for given file in file browser'
        label = find_web_elem(self.web_elem, css_sel, err_msg)
        return label.text

    @property
    def is_selected(self):
        return 'active' in self.web_elem.get_attribute('class')

    @property
    def size(self):
        css_sel = '.file-list-col-size'
        err_msg = 'unable to locate size label for "{}" ' \
                  'in file browser'.format(self.name)
        label = find_web_elem(self.web_elem, css_sel, err_msg)
        return label.text

    @property
    def modification_date(self):
        css_sel = '.file-list-col-modification'
        err_msg = 'unable to locate modification date label for "{}" ' \
                  'in file browser'.format(self.name)
        label = find_web_elem(self.web_elem, css_sel, err_msg)
        return label.text

    @property
    def is_file(self):
        icon = self._get_icon()
        return 'file' in icon.get_attribute('class')

    @property
    def is_directory(self):
        icon = self._get_icon()
        return 'folder' in icon.get_attribute('class')

    @property
    def is_shared(self):
        icon = self._get_icon()
        return 'share' in icon.get_attribute('class')

    def _get_icon(self):
        css_sel = '.file-icon .one-icon'
        err_msg = 'unable to locate file icon for "{}" in ' \
                  'file browser'.format(self.name)
        return find_web_elem(self.web_elem, css_sel, err_msg)

    def click(self):
        err_msg = 'clicking on "{}" in file browser disabled'.format(self.name)
        click_on_web_elem(self._driver, self.web_elem, err_msg)

    def double_click(self):
        ActionChains(self._driver).double_click(self.web_elem).perform()

    def hover_on(self):
        ActionChains(self._driver).move_to_element(self.web_elem).perform()

    def click_on_tool(self, name):
        self.hover_on()
        tool = self._get_tool_icon(name)
        err_msg = 'clicking on "{tool}" for "{name}" in file browser ' \
                  'disabled'.format(tool=name, name=self.name)
        click_on_web_elem(self._driver, tool, err_msg)

    def _get_tool_icon(self, name):
        css_sel = '.file-row-tools .oneicon-{}'.format(name)
        err_msg = 'unable to locate "{tool}" tool in file row for ' \
                  '"{name}"'.format(tool=name, name=self.name)
        return find_web_elem(self.web_elem, css_sel, err_msg)
