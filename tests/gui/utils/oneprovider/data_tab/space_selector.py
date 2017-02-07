"""Utils and fixtures to facilitate operations on space selector in
data tab in oneprovider web GUI.
"""

from tests.gui.utils.common.common import PageObject
from tests.gui.utils.common.mixins import ExpandableMixin, ClickableMixin
from tests.gui.utils.common.web_elements import TextLabelWebElement, \
    IconWebElement, ToggleWebElement

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class SpaceSelector(PageObject, ExpandableMixin):
    selected_space_name = TextLabelWebElement('.item-label')
    _icon = IconWebElement('.item-icon .one-icon')
    _toggle = ToggleWebElement('a.dropdown-toggle')

    def is_selected_space_home(self):
        return 'oneicon-space-home' in self._icon.get_attribute('class')

    def __str__(self):
        return 'space selector in {}'.format(str(self._parent))

    def __iter__(self):
        if self.is_expanded:
            css_sel = 'ul.dropdown-menu-list li'
            return (SpaceRecord(self._driver, space, self) for space
                    in self.web_elem.find_elements_by_css_selector(css_sel))
        else:
            raise RuntimeError('dropdown menu in {} is '
                               'not expanded'.format(str(self)))

    def __getitem__(self, name):
        for space in self:
            if name == space.name:
                return space
        else:
            raise RuntimeError('no space named "{space}" displayed in expanded '
                               'dropdown menu in {item}'.format(space=name,
                                                                item=str(self)))


class SpaceRecord(PageObject, ClickableMixin):
    name = TextLabelWebElement('.item-label', parent_name='given space record')
    _icon = IconWebElement('.item-icon .one-icon')

    def __str__(self):
        return '{name} in {parent}'.format(name=self.name,
                                           parent=str(self._parent))

    def is_home(self):
        return 'oneicon-space-home' in self._icon.get_attribute('class')
