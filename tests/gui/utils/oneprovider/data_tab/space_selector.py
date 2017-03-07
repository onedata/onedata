"""Utils and fixtures to facilitate operations on space selector in
data tab in oneprovider web GUI.
"""

from tests.gui.utils.common.common import PageObject, ExpandableMixin
from tests.gui.utils.common.web_elements import TextLabelWebElement, \
    IconWebElement, ToggleWebElement, WebItemsSequence

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class _SpaceRecord(PageObject):
    name = id = TextLabelWebElement('.item-label',
                                    parent_name='given space record')
    _icon = IconWebElement('.item-icon .one-icon')

    def __str__(self):
        return '{name} in {parent}'.format(name=self.name, parent=self.parent)

    def is_home(self):
        return 'oneicon-space-home' in self._icon.get_attribute('class')


class SpaceSelector(PageObject, ExpandableMixin):
    selected_space_name = TextLabelWebElement('.item-label')
    spaces = WebItemsSequence('ul.dropdown-menu-list li', cls=_SpaceRecord)
    _icon = IconWebElement('.item-icon .one-icon')
    _toggle = ToggleWebElement('a.dropdown-toggle')

    def __str__(self):
        return 'space selector in {}'.format(self.parent)

    def is_selected_space_home(self):
        return 'oneicon-space-home' in self._icon.get_attribute('class')
