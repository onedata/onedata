"""Utils and fixtures to facilitate operations on tokens in Onezone gui"""

from tests.gui.utils.common.common import PageObject
from tests.gui.utils.common.web_elements import InputWebElement, ButtonWebElement

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class TokenRecord(PageObject):
    value = InputWebElement('.token-header input')
    _copy_btn = ButtonWebElement('.oneicon-clipboard-copy')
    _remove_btn = ButtonWebElement('.oneicon-remove')

    def __str__(self):
        return 'token record in {}'.format(self._parent)

    def copy(self):
        self._click_on_btn('copy')

    def remove(self):
        self._click_on_btn('remove')
