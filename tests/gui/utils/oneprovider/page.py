"""Utils and fixtures to facilitate operations on oneprovider web GUI.
"""

from tests.gui.utils.common.mixins import ExpandableMixin
from tests.gui.utils.generic import click_on_web_elem, find_web_elem, find_web_elem_with_text

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class OPpage(object):
    def __init__(self, driver):
        self.web_elem = driver

    @property
    def user_profile(self):
        css_sel = 'li.profile-dropdown'
        err_msg = 'unable to locate user profile toggle in op'
        return UserProfile(self.web_elem,
                           find_web_elem(self.web_elem, css_sel, err_msg))

    def click_on_tab(self, name):
        css_sel = 'nav.primary-sidebar li a'
        err_msg = 'unable to locate {} btn in primary sidebar in op'.format(name)
        btn = find_web_elem_with_text(self.web_elem, css_sel,
                                      name.lower(), err_msg)

        err_msg = 'clicking on {} in primary sidebar in op disabled'.format(name)
        click_on_web_elem(self.web_elem, btn, err_msg)


class UserProfile(ExpandableMixin):
    def __init__(self, driver, web_elem):
        self.web_elem = web_elem
        self._driver = driver

    def _click_on_toggle(self, toggle):
        err_msg = 'clicking on dropdown toggle for user profile in op disabled'
        click_on_web_elem(self._driver, toggle, err_msg)

    def _get_toggle(self):
        css_sel = 'a.dropdown-toggle'
        err_msg = 'unable to locate dropdown toggle for user profile in op'
        return find_web_elem(self.web_elem, css_sel, err_msg)

    def log_out(self):
        self._click_on_dropdown_item('log out')

    def manage_account(self):
        self._click_on_dropdown_item('manage account')

    def _click_on_dropdown_item(self, name):
        if self.is_expanded:
            css_sel = 'ul.dropdown-menu-list li'
            err_msg = 'unable to locate {} in user profile dropdown in op'
            btn = find_web_elem_with_text(self.web_elem, css_sel,
                                          name.lower(), err_msg)

            err_msg = 'clicking on {} in user profile dropdown in op ' \
                      'disabled'.format(name)
            click_on_web_elem(self._driver, btn, err_msg)
        else:
            raise RuntimeError('unexpanded user profile dropdown in op')
