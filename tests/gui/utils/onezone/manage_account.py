"""Utils and fixtures to facilitate operations on MANAGE ACCOUNT Onezone top bar.
"""

from tests.gui.utils.generic import find_web_elem, find_web_elem_with_text

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class ManageAccount(object):

    def __init__(self, web_elem):
        self.web_elem = web_elem

    class AccountDropdown(object):
        def __init__(self, web_elem):
            self.web_elem = web_elem

        def logout(self):
            btn = self._get_btn('logout')
            btn.click()

        def _get_btn(self, name):
            css_sel = 'li a'
            err_msg = 'no button named {btn} found in account dropdown ' \
                      'for MANAGE ACCOUNT in oz panel'.format(btn=name)
            return find_web_elem_with_text(self.web_elem, css_sel,
                                           name, err_msg)

    @property
    def account_dropdown(self):
        if self.is_account_dropdown_expanded:
            css_sel = 'ul.dropdown-menu-list'
            err_msg = 'no account dopdown found in MANAGE ACCOUNT in oz panel'
            return ManageAccount.AccountDropdown(find_web_elem(self.web_elem,
                                                               css_sel, err_msg))
        else:
            raise RuntimeError('account dropdown in MANAGE ACCOUNT '
                               'in oz is not expanded')

    @property
    def is_account_dropdown_expanded(self):
        toggle = self._get_account_dropdown_toggle()
        return self._is_account_dropdown_expanded(toggle)

    def expand_account_dropdown(self):
        toggle = self._get_account_dropdown_toggle()
        if not self._is_account_dropdown_expanded(toggle):
            toggle.click()

    def collapse_account_dropdown(self):
        toggle = self._get_account_dropdown_toggle()
        if self._is_account_dropdown_expanded(toggle):
            toggle.click()

    # noinspection PyMethodMayBeStatic
    def _is_account_dropdown_expanded(self, toggle):
        aria_expanded = toggle.get_attribute('aria-expanded')
        return True if (aria_expanded and 'true' == aria_expanded) else False

    def _get_account_dropdown_toggle(self):
        css_sel = 'li.account-menu a.dropdown-toggle'
        err_msg = 'no account dropdown toggle found in MANAGE ACCOUNT in oz panel'
        return find_web_elem(self.web_elem, css_sel, err_msg)
