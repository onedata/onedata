"""Utils and fixtures to facilitate operations on oneprovider web GUI.
"""

from tests.gui.utils.common.common import PageObject, ExpandableMixin
from tests.gui.utils.common.web_elements import ToggleWebElement, ButtonWithTextWebElement
from tests.gui.utils.generic import click_on_web_elem

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class UserProfile(PageObject, ExpandableMixin):
    _toggle = ToggleWebElement('a.dropdown-toggle')
    _logout_btn = ButtonWithTextWebElement('ul.dropdown-menu-list li',
                                           text='log out')
    _manage_account_btn = ButtonWithTextWebElement('ul.dropdown-menu-list li',
                                                   text='manage account')

    def __str__(self):
        return 'USER PROFILE in {}'.format(self.parent)

    def log_out(self):
        self._click_on_dropdown_item('log out')

    def manage_account(self):
        self._click_on_dropdown_item('manage account')

    def _click_on_dropdown_item(self, name):
        if self.is_expanded():
            btn = getattr(self, '_{}_btn'.format(name.replace(' ', '_')))
            click_on_web_elem(self.driver, btn,
                              lambda: 'cannot click on {} in '
                                      '{}'.format(name, self))
        else:
            raise RuntimeError('unexpanded user profile dropdown in op')
