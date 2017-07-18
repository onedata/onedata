"""Utils to facilitate operations on space selector in
data tab in oneprovider web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.utils.core.base import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import Label, WebElement, WebItemsSequence


class SpaceRecord(PageObject):
    name = id = Label('.item-label', parent_name='given space record')
    _icon = WebElement('.item-icon .one-icon')

    def __str__(self):
        return '{name} in {parent}'.format(name=self.name, parent=self.parent)

    def is_home(self):
        return 'oneicon-space-home' in self._icon.get_attribute('class')


class SpaceSelector(PageObject, ExpandableMixin):
    selected_space_name = Label('.item-label')
    spaces = WebItemsSequence('ul.dropdown-menu-list li', cls=SpaceRecord)
    _icon = WebElement('.item-icon .one-icon')
    _toggle = WebElement('a.dropdown-toggle')

    def is_selected_space_home(self):
        return 'oneicon-space-home' in self._icon.get_attribute('class')
