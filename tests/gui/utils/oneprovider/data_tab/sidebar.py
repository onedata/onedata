# coding=utf-8
"""Utils and fixtures to facilitate operations on sidebar in
data tab in oneprovider web GUI.
"""

from selenium.webdriver import ActionChains

from tests.gui.utils.common.common import PageObject, ExpandableMixin
from tests.gui.utils.common.web_elements import ToggleWebElement, TextLabelWebElement, \
    WebElement, ItemListWebElement, HeaderWebElement
from tests.gui.utils.oneprovider.data_tab.space_selector import SpaceSelector

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class DataTabSidebar(PageObject):
    _space_selector = WebElement('.data-spaces-select')
    _root_dir = ItemListWebElement('.data-files-tree ul li')

    def __init__(self, *args, **kwargs):
        self._resize_handler = kwargs.pop('resize_handler')
        super(DataTabSidebar, self).__init__(*args, **kwargs)

    def __str__(self):
        return 'sidebar in {}'.format(self._parent)

    @property
    def space_selector(self):
        return SpaceSelector(self._driver, self._space_selector, self)

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
        root, root_content = self._root_dir[:2]
        return DirectoryTree(self._driver, root, self, children=root_content)

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
    name = TextLabelWebElement('.item-label')
    _toggle = ToggleWebElement('.item-icon .one-icon')
    _header = HeaderWebElement('.secondary-sidebar-item.dir-item')
    _click_area = WebElement('.secondary-sidebar-item.dir-item '
                             '.item-click-area')

    def __init__(self, *args, **kwargs):
        self._children = kwargs.pop('children')
        super(DirectoryTree, self).__init__(*args, **kwargs)

    def __str__(self):
        return 'DirectoryTree({path}) in {parent}'.format(path=self.pwd(),
                                                          parent=self._parent)

    def __iter__(self):
        css_sel = 'ul.data-files-tree-list li'
        return (DirectoryTree(self._driver, dir_tree, self, children=dir_tree)
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
        if not isinstance(self._parent, DirectoryTree):
            return '/'
        else:
            return '{path}{dir}/'.format(path=self._parent.pwd(),
                                         dir=self.name)

    @property
    def displayed_name(self):
        css_id = self._header.get_attribute('id')
        self._driver.execute_script(_jquery_fun)
        js_get_name = "return $('#{}').getShowingText();".format(css_id)
        return self._driver.execute_script(js_get_name).strip()


_jquery_fun = """
jQuery.fn.getShowingText = function () {
    // Add temporary element for measuring character widths
    $('body').append('<div id="Test" style="padding:0;border:0;height:auto;width:auto;position:absolute;display:none;"></div>');
    var longString = $(this).text();
    var eleWidth = $(this).innerWidth();
    var totalWidth = 0;
    var totalString = '';
    var finished = false;
    var ellipWidth = $('#Test').html('&hellip;').innerWidth();
    var offset = 7; // seems to differ based on browser (6 for Chrome and 7 for Firefox?)
    for (var i = 0;
    (i < longString.length) && ((totalWidth) < (eleWidth-offset)); i++) {
        $('#Test').text(longString.charAt(i));
        totalWidth += $('#Test').innerWidth();
        totalString += longString.charAt(i);
        if(i+1 === longString.length)
        {
            finished = true;
        }
    }
    $('body').remove('#Test'); // Clean up temporary element
    if(finished === false)
    {
        return totalString.substring(0,totalString.length-3)+"…";
    }
    else
    {
        return longString;
    }
}
"""
