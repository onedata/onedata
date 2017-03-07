"""Utils and fixtures to facilitate operations on tokens in Onezone gui"""

from tests.gui.utils.common.common import PageObject
from tests.gui.utils.common.web_elements import InputWebElement, ButtonWebElement, WebItemsSequence, \
    ButtonWithTextWebElement
from tests.gui.utils.onezone.panels import OZPanel

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class AccessTokensPanel(OZPanel):
    tokens = WebItemsSequence('.tokens-list-item', cls=_TokenRecord)
    _create_token_btn = ButtonWithTextWebElement('.clickable',
                                                 text='create new access token')

    def create_new_access_token(self):
        self._click_on_btn('create_token')


class _TokenRecord(PageObject):
    value = InputWebElement('.token-header input')
    _copy_btn = ButtonWebElement('.oneicon-clipboard-copy')
    _remove_btn = ButtonWebElement('.oneicon-remove')

    def __str__(self):
        return 'token record in {}'.format(self.parent)

    def copy(self):
        self._click_on_btn('copy')

    def remove(self):
        self._click_on_btn('remove')
