"""Utils and fixtures to facilitate operations on oneprovider web GUI.
"""

from tests.gui.utils.core.base import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import WebElement, NamedButton

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class UserProfile(PageObject, ExpandableMixin):
    log_out = NamedButton('ul.dropdown-menu-list li', text='log out')
    manage_account = NamedButton('ul.dropdown-menu-list li',
                                 text='manage account')
    _toggle = WebElement('a.dropdown-toggle')

    def __str__(self):
        return 'USER PROFILE in {}'.format(self.parent)
