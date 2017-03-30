"""Utils and fixtures to facilitate operations on world map in Onezone web GUI.
"""

from tests.gui.utils.common.common import PageObject, ExpandableMixin
from tests.gui.utils.common.web_elements import TextLabelWebElement, ToggleWebElement, \
    WebElement, ButtonWebElement, IconWebElement, WebItemsSequence
from tests.gui.utils.generic import click_on_web_elem
from .common import OZPanel

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class _SpaceRecord(PageObject):
    name = id = TextLabelWebElement('.one-label.truncate',
                                    parent_name='provider popup')
    size = TextLabelWebElement('.space-header-size')
    _space_icon = IconWebElement('.oneicon')

    def __str__(self):
        return '"{}" space record in {}'.format(self.name, self.parent)

    def is_home(self):
        return 'default' in self._space_icon.get_attribute('class')


class _ProviderRecord(PageObject, ExpandableMixin):
    name = id = TextLabelWebElement('.provider-header.truncate',
                                    parent_name='given provider record')
    spaces_count = TextLabelWebElement('.spaces-count')
    spaces = WebItemsSequence('ul.tertiary-list li.sidebar-provider-space',
                              cls=_SpaceRecord)
    _provider_icon = IconWebElement('.provider-icon .oneicon')
    _toggle = ToggleWebElement('.spaces-count')
    _click_area = WebElement('.secondary-item-container')
    _set_home_btn = ButtonWebElement('.secondary-item-element.star-toggle '
                                     '.oneicon-home-outline')
    _home_icon = WebElement('.secondary-item-element.star-toggle '
                            '.oneicon-home')

    def __str__(self):
        return 'provider record named: "{}" in {}'.format(self.name,
                                                          self.parent)

    def is_home(self):
        home1 = 'provider-home' in self._provider_icon.get_attribute('class')
        try:
            _ = self._home_icon
        except RuntimeError:
            return False
        else:
            return True and home1

    def set_as_home(self):
        if not self.is_home():
            self._click_on_btn('set_home')

    def unset_from_home(self):
        if self.is_home():
            click_on_web_elem(self.driver, self._home_icon,
                              'cannot click on home icon for {item} in '
                              '{parent}'.format(item=self, parent=self.parent))

    def is_working(self):
        return 'color-provider-online' \
               in self._provider_icon.get_attribute('class')

    def is_not_working(self):
        return 'color-provider-offline' \
               in self._provider_icon.get_attribute('class')


class GoToYourFilesPanel(OZPanel):
    providers = WebItemsSequence('#providers-list .providers-accordion-item',
                                 cls=_ProviderRecord)
