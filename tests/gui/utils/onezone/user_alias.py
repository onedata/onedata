"""Utils and fixtures to facilitate operations on USER ALIAS panel
in Onezone web GUI.
"""

from tests.gui.utils.core.web_elements import Label, Button, WebItem
from .common import OZPanel, EditBox

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class UserAliasPanel(OZPanel):
    alias = Label('.alias-text')
    edit_box = WebItem('.alias-accordion-toggle.clickable', cls=EditBox)
    rename = Button('.oneicon-rename')
