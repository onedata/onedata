"""Utils and fixtures to facilitate operations on sidebar in
data tab in oneprovider web GUI.
"""

from selenium.webdriver import ActionChains

from tests.gui.utils.common.expandable import Expandable
from tests.gui.utils.generic import click_on_web_elem, find_web_elem
from tests.gui.utils.oneprovider.data_tab.space_selector import SpaceSelector

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class DataTabSidebar(object):
    def __init__(self, driver, web_elem, resize_handler):
        self._driver = driver
        self.web_elem = web_elem
        self._resize_handler = resize_handler

    @property
    def space_selector(self):
        css_sel = '.data-spaces-select'
        err_msg = 'unable to locate space selector in sidebar in data tab in op'
        selector = find_web_elem(self.web_elem, css_sel, err_msg)
        return SpaceSelector(self._driver, selector)

    @property
    def width(self):
        return self._resize_handler.location['x']

    @width.setter
    def width(self, value):
        offset = value - self.width
        action = ActionChains(self._driver)
        action.drag_and_drop_by_offset(self._resize_handler, offset, 0)
        action.perform()

    @property
    def root_dir(self):
        css_sel = '.data-files-tree ul li'
        items = self.web_elem.find_elements_by_css_selector(css_sel)
        if len(items) < 2:
            raise RuntimeError('unable to locate root dir and its children in '
                               'directory tree sidebar in data tab in op')
        return DirectoryTree(self._driver, items[0], None, items[1])

    @property
    def cwd(self):
        cwd = self._cwd(self.root_dir)
        if cwd:
            return cwd
        else:
            raise RuntimeError('no working directory found')

    def _cwd(self, curr_dir):
        if curr_dir.is_active:
            return curr_dir
        for directory in curr_dir.subdirectories:
            cwd = self._cwd(directory)
            if cwd:
                return cwd


class DirectoryTree(Expandable):
    def __init__(self, driver, web_elem, parent, children):
        self._driver = driver
        self.web_elem = web_elem
        self._parent = parent
        self._children = children

    @property
    def name(self):
        err_msg = 'unable to locate name label for given directory in ' \
                  'data tab sidebar in op'
        label = find_web_elem(self.web_elem, '.item-label', err_msg)
        return label.text

    @property
    def is_active(self):
        css_sel = '.secondary-sidebar-item.dir-item'
        err_msg = 'unable to locate header area for given directory in ' \
                  'data tab sidebar in op'
        item = find_web_elem(self.web_elem, css_sel, err_msg)
        return 'active' in item.get_attribute('class')

    def _click_on_toggle(self, toggle):
        err_msg = 'clicking on directory icon for "{}" directory in data tab ' \
                  'sidebar in op is disabled'.format(self.pwd())
        click_on_web_elem(self._driver, toggle, err_msg)

    def _get_toggle(self):
        css_sel = '.item-icon .one-icon'
        err_msg = 'unable to locate icon for directory "{}" in data tab ' \
                  'sidebar in op'.format(self.pwd())
        return find_web_elem(self.web_elem, css_sel, err_msg)

    # noinspection PyMethodMayBeStatic
    def _is_expanded(self, toggle):
        return True if 'folder-open' in toggle.get_attribute('class') else False

    def pwd(self):
        if not self._parent:
            return '/'
        else:
            return '{path}{dir}/'.format(path=self._parent.pwd(),
                                         dir=self.name)

    def click(self):
        css_sel = '.secondary-sidebar-item.dir-item .item-click-area'
        err_msg = 'unable to locate click area for "{}" directory in ' \
                  'data tab sidebar in op'.format(self.pwd())
        item = find_web_elem(self.web_elem, css_sel, err_msg)
        click_on_web_elem(self._driver, item, 'clicking on {} directory '
                                              'disabled'.format(self.pwd()))

    @property
    def subdirectories(self):
        css_sel = 'ul.data-files-tree-list li'
        return [DirectoryTree(self._driver, dir_tree, self, dir_tree)
                for dir_tree
                in self._children.find_elements_by_css_selector(css_sel)]

    def __getitem__(self, name):
        if not self.is_expanded:
            self.expand()
        for directory in self.subdirectories:
            if directory.name == name:
                return directory
        else:
            raise RuntimeError('no subdirectory named "{name}" found '
                               'in {path}'.format(name=name, path=self.pwd()))
