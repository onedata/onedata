"""Utils and fixtures to facilitate operations on sidebar in
data tab in oneprovider web GUI.
"""

from selenium.webdriver import ActionChains

from tests.gui.utils.core.base import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import Label, WebElement, WebItem, WebElementsSequence
from tests.gui.utils.oneprovider.data_tab.space_selector import SpaceSelector

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class DataTabSidebar(PageObject):
    space_selector = WebItem('.data-spaces-select', cls=SpaceSelector)
    _root_dir = WebElementsSequence('.data-files-tree ul li')

    def __init__(self, *args, **kwargs):
        self._resize_handler = kwargs.pop('resize_handler')
        super(DataTabSidebar, self).__init__(*args, **kwargs)

    def __str__(self):
        return 'sidebar in {}'.format(self.parent)

    @property
    def width(self):
        return self._resize_handler.location['x']

    @width.setter
    def width(self, value):
        offset = value - self.width
        action = ActionChains(self.driver)
        action.drag_and_drop_by_offset(self._resize_handler, offset, 0)
        action.perform()

    @property
    def root_dir(self):
        root, root_content = self._root_dir[:2]
        return DirectoryTree(self.driver, root, self, children=root_content)

    @property
    def cwd(self):
        cwd = self._cwd(self.root_dir)
        if cwd:
            return cwd
        else:
            raise RuntimeError('no working directory found')

    def _cwd(self, curr_dir):
        if curr_dir.is_active():
            return curr_dir
        for directory in curr_dir:
            cwd = self._cwd(directory)
            if cwd:
                return cwd


class DirectoryTree(PageObject, ExpandableMixin):
    name = Label('.item-label')
    _toggle = WebElement('.item-icon .one-icon')
    _header = WebElement('.secondary-sidebar-item.dir-item')
    _click_area = WebElement('.secondary-sidebar-item.dir-item '
                             '.item-click-area')
    _header_label = WebElement('.secondary-sidebar-item.dir-item '
                               '.truncate-secondary-sidebar-item')

    def __init__(self, *args, **kwargs):
        self._children = kwargs.pop('children')
        super(DirectoryTree, self).__init__(*args, **kwargs)

    def __str__(self):
        return 'DirectoryTree({path}) in {parent}'.format(path=self.pwd(),
                                                          parent=self.parent)

    def __iter__(self):
        css_sel = 'ul.data-files-tree-list li'
        return (DirectoryTree(self.driver, dir_tree, self, children=dir_tree)
                for dir_tree in
                self._children.find_elements_by_css_selector(css_sel))

    def __getitem__(self, name):
        for directory in self:
            if directory.name == name:
                return directory
        else:
            raise RuntimeError('no subdirectory named "{name}" found '
                               'in {path}'.format(name=name, path=self))

    def is_expanded(self):
        return True if 'open' in self._toggle.get_attribute('class') else False

    def is_active(self):
        return 'active' in self._header.get_attribute('class')

    def pwd(self):
        if not isinstance(self.parent, DirectoryTree):
            return '/'
        else:
            return '{path}{dir}/'.format(path=self.parent.pwd(),
                                         dir=self.name)

    @property
    def displayed_name_width(self):
        return self.driver.execute_script("return $(arguments[0]).width();",
                                          self._header_label)
