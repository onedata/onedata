"""Utils and fixtures to facilitate operations on edit box (e.g. in USER ALIAS panel)
in Onezone web GUI.
"""

from tests.gui.utils.common.common import PageObject
from tests.gui.utils.common.web_elements import InputWebElement, \
    ButtonWebElement, web_item
from tests.gui.utils.generic import click_on_web_elem

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@web_item
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

    def _click_on_btn(self, btn_type):
        btn = getattr(self, '_{}_btn'.format(btn_type))
        err_msg = 'clicking on {} btn in edit box disabled'.format(btn_type)
        click_on_web_elem(self._driver, btn, err_msg)
