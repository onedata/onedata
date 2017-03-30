"""Utils and fixtures to facilitate operations on USER ALIAS panel
in Onezone web GUI.
"""

from selenium.common.exceptions import NoSuchElementException

from tests.gui.utils.core.web_elements import TextLabelWebElement, ButtonWebElement, WebItem
from .common import OZPanel, EditBox
from ..generic import suppress, click_on_web_elem

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class UserAliasPanel(OZPanel):
    alias = TextLabelWebElement('.alias-text')
    edit_box = WebItem('.alias-accordion-toggle.clickable', cls=EditBox)

    _rename_btn = ButtonWebElement('.oneicon-rename')

    def edit(self):
        with suppress(NoSuchElementException):
            err_msg = 'clicking on rename btn in {} disabled'.format(self)
            click_on_web_elem(self.driver, self._rename_btn, err_msg)
