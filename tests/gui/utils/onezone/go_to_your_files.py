"""Utils and fixtures to facilitate operations on world map in Onezone web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.utils.core.base import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import Label, WebElement, Button, WebItemsSequence
from .common import OZPanel


class SpaceRecord(PageObject):
    name = id = Label('.one-label.truncate', parent_name='provider popup')
    size = Label('.space-header-size')
    _space_icon = WebElement('.oneicon')

    def __str__(self):
        return '"{}" space record in {}'.format(self.name, self.parent)

    def is_home(self):
        return 'default' in self._space_icon.get_attribute('class')


class ProviderRecord(PageObject, ExpandableMixin):
    name = id = Label('.provider-header.truncate',
                      parent_name='given provider record')
    spaces_count = Label('.spaces-count')
    set_as_home = Button('.secondary-item-element.star-toggle '
                         '.oneicon-home-outline')
    unset_from_home = Button('.secondary-item-element.star-toggle '
                             '.oneicon-home')
    spaces = WebItemsSequence('ul.tertiary-list li.sidebar-provider-space',
                              cls=SpaceRecord)
    _provider_icon = WebElement('.provider-icon .oneicon')
    _toggle = WebElement('.spaces-count')
    _click_area = WebElement('.secondary-item-container')

    def __str__(self):
        return 'provider record named: "{}" in {}'.format(self.name, self.parent)

    def is_home(self):
        home1 = 'provider-home' in self._provider_icon.get_attribute('class')
        try:
            _ = self.unset_from_home
        except RuntimeError:
            return False
        else:
            return True and home1

    def is_working(self):
        return ('color-provider-online' in
                self._provider_icon.get_attribute('class'))

    def is_not_working(self):
        return ('color-provider-offline' in
                self._provider_icon.get_attribute('class'))


class GoToYourFilesPanel(OZPanel):
    providers = WebItemsSequence('#providers-list .providers-accordion-item',
                                 cls=ProviderRecord)
