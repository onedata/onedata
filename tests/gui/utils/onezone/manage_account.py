"""Utils and fixtures to facilitate operations on MANAGE ACCOUNT Onezone top bar.
"""

from tests.gui.utils.core.base import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import WebElement, NamedButton

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class ManageAccount(PageObject, ExpandableMixin):
    logout = NamedButton('ul.dropdown-menu-list li a', text='logout')
    _toggle = WebElement('li.account-menu a.dropdown-toggle')

    def __str__(self):
        return 'Manage Account in {}'.format(self.parent)
