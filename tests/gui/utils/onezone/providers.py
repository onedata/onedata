"""Utils and fixtures to facilitate operations on world map in Onezone web GUI.
"""

from selenium.common.exceptions import NoSuchElementException

from tests.gui.utils.common.common import PageObject, ExpandableMixin
from tests.gui.utils.common.web_elements import TextLabelWebElement, ToggleWebElement, \
    WebElement, ItemListWebElement, ButtonWebElement, IconWebElement
from tests.gui.utils.generic import find_web_elem, click_on_web_elem

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class ProviderRecord(PageObject, ExpandableMixin):
    name = TextLabelWebElement('.provider-header.truncate',
                               parent_name='given provider record')
    spaces_count = TextLabelWebElement('.spaces-count')
    _toggle = ToggleWebElement('.spaces-count')
    _supported_spaces = ItemListWebElement('ul.tertiary-list '
                                           'li.sidebar-provider-space')
    _set_home_btn = ButtonWebElement('.secondary-item-element.star-toggle '
                                     '.oneicon-home-outline')
    _home_space_icon = WebElement('.oneicon-provider-home')
    _home_icon = WebElement('.secondary-item-element.star-toggle .oneicon-home')
    _working_icon = IconWebElement('.provider-icon .color-provider-online')
    _not_working_icon = IconWebElement('.provider-icon '
                                       '.color-provider-offline')
    _click_area = WebElement('.secondary-item-container')

    def __str__(self):
        return 'provider record named: "{}" in {}'.format(self.name,
                                                          str(self._parent))

    def __iter__(self):
        if self.is_expanded():
            return (SpaceRecordInProvidersPanel(space,
                                                name_css='.one-label.truncate',
                                                size_css='.space-header-size')
                    for space in self._supported_spaces)
        else:
            raise RuntimeError('submenu for provider named "{}" '
                               'is not expanded in GO TO YOUR FILES '
                               'panel'.format(self.name))

    def __getitem__(self, space_name):
        for space in self:
            if space_name == space.name:
                return space
        else:
            raise RuntimeError('no supported space named "{space}" '
                               'for provider named "{provider}" in '
                               'GO TO YOUR FILES panel '
                               'found'.format(provider=self.name,
                                              space=space_name))

    def is_home(self):
        try:
            _ = self._home_icon and self._home_space_icon
        except RuntimeError:
            return False
        else:
            return True

    def set_as_home(self):
        if not self.is_home():
            self._click_on_btn('set_home')

    def unset_from_home(self):
        err_msg = 'cannot click on home icon for provider record named ' \
                  '"{}" in GO TO YOUR FILES panel'.format(self.name)
        click_on_web_elem(self._driver, self._home_icon, err_msg)

    def is_working(self):
        try:
            _ = self._working_icon
        except RuntimeError:
            return False
        else:
            return True

    def is_not_working(self):
        try:
            _ = self._not_working_icon
        except RuntimeError:
            return False
        else:
            return True


class SpaceRecordInProvidersPanel(object):
    def __init__(self, web_elem, name_css, size_css):
        self.web_elem = web_elem
        self.name_css = name_css
        self.size_css = size_css

    @property
    def name(self):
        err_msg = 'unable to locate name header for supported space in ' \
                  'displayed provider panel'
        header = find_web_elem(self.web_elem, self.name_css, err_msg)
        return header.text

    @property
    def size(self):
        err_msg = 'no size label for space named "{}" found ' \
                  'in displayed provider panel'.format(self.name)
        size_label = find_web_elem(self.web_elem, self.size_css, err_msg)
        return size_label.text

    @property
    def is_home(self):
        css_sel = '.oneicon-space-default'
        try:
            self.web_elem.find_element_by_css_selector(css_sel)
        except NoSuchElementException:
            return False
        else:
            return True
