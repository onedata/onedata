"""Utils and fixtures to facilitate operations on USER ALIAS panel in Onezone web GUI.
"""

from selenium.common.exceptions import NoSuchElementException

from tests.gui.utils.generic import find_web_elem, suppress
from tests.gui.utils.onezone.sidebar_panel import OZPanel
from tests.gui.utils.onezone.edit_box import EditBox


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class UserAliasPanel(OZPanel):

    @property
    def alias(self):
        css_sel = '.alias-text'
        err_msg = 'no alias found in USER ALIAS oz panel'
        header = find_web_elem(self.web_elem, css_sel, err_msg)
        return header.text

    def edit_alias(self):
        css_sel = '.alias-accordion-toggle.clickable'
        err_msg = 'no clickable alias header or edit box ' \
                  'found in USER ALIAS oz panel'
        edit_box = find_web_elem(self.web_elem, css_sel, err_msg)

        with suppress(NoSuchElementException):
            edit_box.find_element_by_css_selector('.oneicon-rename').click()

        return EditBox(edit_box)
