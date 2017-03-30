"""Utils and fixtures to facilitate operations on world map in Onezone web GUI.
"""

from ..common.common import PageObject
from ..common.web_elements import TextLabelWebElement, InputWebElement, \
    ButtonWebElement, WebElement, WebItemsSequence, WebItem, IconWebElement

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class _SpaceRecord(PageObject):
    name = id = TextLabelWebElement('.space-label',
                                    parent_name='provider popup')
    size = TextLabelWebElement('.space-size')
    _space_icon = IconWebElement('.space-icon .oneicon')

    def __str__(self):
        return '"{}" space record in {}'.format(self.name, self.parent)

    def is_home(self):
        return 'default' in self._space_icon.get_attribute('class')


class _ProviderPopup(PageObject):
    hostname = InputWebElement('input.provider-host-text')
    name = id = TextLabelWebElement('.title-label',
                                    parent_name='given provider popup')
    spaces = WebItemsSequence('ul li.provider-place-drop-space',
                              cls=_SpaceRecord)
    _cp_hostname_btn = ButtonWebElement('.provider-host-copy-btn')
    _go_to_files_btn = ButtonWebElement('.drop-body button')
    _popup = WebElement('.provider-place-drop')

    def __str__(self):
        return '"{}" provider popup on {}'.format(self.name, self.parent)

    def is_working(self):
        return 'working' in self.web_elem.get_attribute('class')

    def is_displayed(self):
        try:
            _ = self._popup
        except RuntimeError:
            return False
        else:
            return True

    def copy_hostname(self):
        self._click_on_btn('cp_hostname')

    def go_to_your_files(self):
        self._click_on_btn('go_to_files')


class _Message(PageObject):
    title = TextLabelWebElement('.panel-heading')
    msg = TextLabelWebElement('.panel-body')

    def __str__(self):
        return 'Message modal on {}'.format(self.parent)


class WorldMap(PageObject):
    providers = WebItemsSequence('.provider-place', cls=_ProviderPopup)
    message = WebItem('.panel-onezone-alert', cls=_Message)

    def __str__(self):
        return 'World Map on {}'.format(self.parent)

    def get_provider_with_displayed_popup(self):
        for provider in self.providers:
            if provider.is_displayed():
                return provider
