"""This module contains gherkin steps to run acceptance tests featuring
account management in onezone web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


from pytest_bdd import parsers, when, then

from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed


@when(parsers.parse('user of {browser_id} expands account settings '
                    'dropdown in "ACCOUNT MANAGE" Onezone top bar'))
@then(parsers.parse('user of {browser_id} expands account settings '
                    'dropdown in "ACCOUNT MANAGE" Onezone top bar'))
@repeat_failed(timeout=WAIT_FRONTEND)
def expand_account_settings_in_oz(selenium, browser_id, oz_page):
    oz_page(selenium[browser_id])['manage account'].expand()


@when(parsers.re(r'user of (?P<browser_id>.+?) clicks on (?P<option>LOGOUT) '
                 r'item in expanded settings dropdown in "ACCOUNT MANAGE" '
                 r'Onezone top bar'))
@then(parsers.re(r'user of (?P<browser_id>.+?) clicks on (?P<option>LOGOUT) '
                 r'item in expanded settings dropdown in "ACCOUNT MANAGE" '
                 r'Onezone top bar'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_on_option_in_account_settings_in_oz(selenium, browser_id,
                                              option, oz_page):
    getattr(oz_page(selenium[browser_id])['manage account'],
            option.lower()).click()
