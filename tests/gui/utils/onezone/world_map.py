"""Utils and fixtures to facilitate operations on world map in Onezone web GUI.
"""

from itertools import islice

from tests.gui.utils.common.common import PageObject
from tests.gui.utils.common.web_elements import TextLabelWebElement, \
    ModalWebElement, ItemListWebElement, InputWebElement, ButtonWebElement, \
    WebElement
from tests.gui.utils.onezone.providers import SpaceRecordInProvidersPanel


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class WorldMap(PageObject):
    _msg_modal = ModalWebElement('.panel-onezone-alert')
    _providers_circles = ItemListWebElement('.provider-place')

    def __str__(self):
        return 'World Map on {}'.format(self._parent)

    def __iter__(self):
        return (ProviderPopup(self._driver, provider, self)
                for provider in self._providers_circles)

    def __getitem__(self, idx):
        providers = islice(self, idx, None)
        try:
            provider = providers.next()
        except StopIteration:
            raise RuntimeError('there is no {index} provider '
                               'provider on map'.format(index=idx))
        else:
            return provider

    @property
    def message(self):
        return Message(self._driver, self._msg_modal, self)

    def get_provider_with_displayed_panel(self):
        for provider in self:
            if provider.is_displayed:
                return provider


class ProviderPopup(PageObject):
    hostname = InputWebElement('input.provider-host-text')
    name = TextLabelWebElement('.title-label',
                               parent_name='given provider popup')
    _supported_spaces = ItemListWebElement('ul li.provider-place-drop-space')
    _cp_hostname_btn = ButtonWebElement('.provider-host-copy-btn')
    _go_to_files_btn = ButtonWebElement('.drop-body button')
    _provider_popup = WebElement('.provider-place-drop')

    def __str__(self):
        return 'provider popup named "{}" on {}'.format(self.name,
                                                        str(self._parent))

    def is_working(self):
        return 'working' in self.web_elem.get_attribute('class')

    def is_displayed(self):
        try:
            _ = self._provider_popup
        except RuntimeError:
            return False
        else:
            return True

    def copy_hostname(self):
        self._click_on_btn('cp_hostname')

    def go_to_your_files(self):
        self._click_on_btn('go_to_files')

    def __iter__(self):
        return (SpaceRecordInProvidersPanel(space, name_css='.space-label',
                                            size_css='.space-size')
                for space in self._supported_spaces)

    def __getitem__(self, space_name):
        for space in self:
            if space_name == space.name:
                return space
        else:
            raise RuntimeError('no supported space named "{space}" '
                               'for {item} found'.format(space=space_name,
                                                         item=str(self)))


class Message(PageObject):
    title = TextLabelWebElement('.panel-heading')
    msg = TextLabelWebElement('.panel-body')

    def __str__(self):
        return 'Message modal on {}'.format(self._parent)
