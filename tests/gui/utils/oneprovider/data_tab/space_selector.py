"""Utils and fixtures to facilitate operations on space selector in
data tab in oneprovider web GUI.
"""

from tests.gui.utils.common.common import PageObject, ExpandableMixin
from tests.gui.utils.common.web_elements import TextLabelWebElement, \
    IconWebElement, ToggleWebElement, ItemListWebElement

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class SpaceSelector(PageObject, ExpandableMixin):
    selected_space_name = TextLabelWebElement('.item-label')
    _icon = IconWebElement('.item-icon .one-icon')
    _toggle = ToggleWebElement('a.dropdown-toggle')
    _spaces = ItemListWebElement('ul.dropdown-menu-list li')

    def __str__(self):
        return 'space selector in {}'.format(self.parent)

    def __iter__(self):
        if self.is_expanded():
            return (SpaceRecord(self.driver, space, self)
                    for space in self._spaces)
        else:
            raise RuntimeError('{} is not expanded'.format(self))

    def __getitem__(self, name):
        if self.is_expanded():
            for space in self:
                if name == space.name:
                    return space
            else:
                raise RuntimeError('no space named "{}" displayed in expanded '
                                   'dropdown menu in {}'.format(name, self))
        else:
            raise RuntimeError('{} should be expanded but is not'.format(self))

    def is_selected_space_home(self):
        return 'oneicon-space-home' in self._icon.get_attribute('class')


class SpaceRecord(PageObject):
    name = TextLabelWebElement('.item-label', parent_name='given space record')
    _icon = IconWebElement('.item-icon .one-icon')

    def __str__(self):
        return '{name} in {parent}'.format(name=self.name, parent=self.parent)

    def is_home(self):
        return 'oneicon-space-home' in self._icon.get_attribute('class')
