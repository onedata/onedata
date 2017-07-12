"""Utils and fixtures to facilitate operations on world map in Onezone web GUI.
"""

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import Label, Input, Button, WebElement, \
    WebItemsSequence, WebItem

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class _SpaceRecord(PageObject):
    name = id = Label('.space-label', parent_name='provider popup')
    size = Label('.space-size')
    _space_icon = WebElement('.space-icon .oneicon')

    def __str__(self):
        return '"{}" space record in {}'.format(self.name, self.parent)

    def is_home(self):
        return 'default' in self._space_icon.get_attribute('class')


class _ProviderPopup(PageObject):
    name = id = Label('.title-label', parent_name='given provider popup')
    hostname = Input('input.provider-host-text')
    copy_hostname = Button('.provider-host-copy-btn')
    spaces = WebItemsSequence('ul li.provider-place-drop-space',
                              cls=_SpaceRecord)
    go_to_your_files = Button('.drop-body .btn-go-to-files, .drop-body button')
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


class _Message(PageObject):
    title = Label('.panel-heading')
    msg = Label('.panel-body')


class WorldMap(PageObject):
    message = WebItem('.panel-onezone-alert', cls=_Message)
    providers = WebItemsSequence('.provider-place', cls=_ProviderPopup)

    def get_provider_with_displayed_popup(self):
        for provider in self.providers:
            if provider.is_displayed():
                return provider
