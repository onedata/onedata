"""Utils and fixtures to facilitate operations on panels (base for specific panels)
in Onezone web GUI.
"""

from ..common.common import PageObject, ExpandableMixin
from ..common.web_elements import ToggleWebElement, TextLabelWebElement, \
    InputWebElement, ButtonWebElement

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class OZPanel(PageObject, ExpandableMixin):
    name = TextLabelWebElement('a.main-accordion-toggle',
                               parent_name='oz panel')
    _toggle = ToggleWebElement('a.main-accordion-toggle')

    def __str__(self):
        return '{} panel in {}'.format(self.name, self.parent)


class EditBox(PageObject):
    value = InputWebElement('input')
    _confirm_btn = ButtonWebElement('.oneicon-checkbox-check')
    _cancel_btn = ButtonWebElement('.oneicon-checkbox-x')

    def __str__(self):
        return 'edit box in {}'.format(self.parent)

    def confirm(self):
        self._click_on_btn('confirm')

    def cancel(self):
        self._click_on_btn('cancel')
