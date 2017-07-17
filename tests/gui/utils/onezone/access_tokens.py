"""Utils and fixtures to facilitate operations on tokens in Onezone gui"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import Input, Button, NamedButton, WebItemsSequence
from .common import OZPanel


class TokenRecord(PageObject):
    value = id = Input('.token-header input')
    copy = Button('.oneicon-clipboard-copy')
    remove = Button('.oneicon-remove')


class AccessTokensPanel(OZPanel):
    tokens = WebItemsSequence('.tokens-list-item', cls=TokenRecord)
    create_new_access_token = NamedButton('.clickable',
                                          text='create new access token')
