"""Utils and fixtures to facilitate operations on edit box (e.g. in USER ALIAS panel)
in Onezone web GUI.
"""

from tests.gui.utils.common.common import PageObject
from tests.gui.utils.common.web_elements import InputWebElement, ButtonWebElement

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class EditBox(PageObject):
    value = InputWebElement('input')
    _confirm_btn = ButtonWebElement('.oneicon-checkbox-check')
    _cancel_btn = ButtonWebElement('.oneicon-checkbox-x')

    def __str__(self):
        return 'edit box in {}'.format(str(self._parent))

    def confirm_input(self):
        self._click_on_btn('confirm')

    def cancel_input(self):
        self._click_on_btn('cancel')
