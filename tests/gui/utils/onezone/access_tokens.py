"""Utils and fixtures to facilitate operations on tokens in Onezone gui"""

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import Input, Button, NamedButton, WebItemsSequence
from .common import OZPanel

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class _TokenRecord(PageObject):
    value = id = Input('.token-header input')
    copy = Button('.oneicon-clipboard-copy')
    remove = Button('.oneicon-remove')

    def __str__(self):
        return 'token record in {}'.format(self.parent)


class AccessTokensPanel(OZPanel):
    tokens = WebItemsSequence('.tokens-list-item', cls=_TokenRecord)
    create_new_access_token = NamedButton('.clickable',
                                          text='create new access token')
