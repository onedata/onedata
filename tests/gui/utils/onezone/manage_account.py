"""Utils and fixtures to facilitate operations on MANAGE ACCOUNT Onezone top bar.
"""

from tests.gui.utils.core.common import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import ToggleWebElement, ButtonWithTextWebElement

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class ManageAccount(PageObject, ExpandableMixin):
    _toggle = ToggleWebElement('li.account-menu a.dropdown-toggle')
    _logout_btn = ButtonWithTextWebElement('ul.dropdown-menu-list li a',
                                           text='logout')

    def __str__(self):
        return 'Manage Account in {}'.format(self.parent)

    def logout(self):
        self._click_on_btn('logout')
