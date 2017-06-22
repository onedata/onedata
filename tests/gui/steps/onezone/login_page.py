"""Steps for Login page Onezone.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from pytest_bdd import given, parsers, then, when

from tests.gui.conftest import WAIT_BACKEND
from tests.gui.utils.generic import repeat_failed, find_web_elem


def _enter_user_credentials_in_login_modal(driver, modals, credentials):
    modal = modals(driver).login
    modal.username = credentials.username
    modal.password = credentials.password


@when(parsers.parse('user of {browser_id} enters credentials of "{user}" '
                    'in "Login with username and password" modal'))
@then(parsers.parse('user of {browser_id} enters credentials of "{user}" '
                    'in "Login with username and password" modal'))
@repeat_failed(timeout=WAIT_BACKEND)
def wt_enter_user_credentials_in_login_modal(selenium, browser_id, user,
                                             users, modals):
    driver = selenium[browser_id]
    _enter_user_credentials_in_login_modal(driver, modals, users[user])


@given(parsers.parse('user of {browser_id} entered credentials of "{user}" '
                     'in "Login with username and password" modal'))
@repeat_failed(timeout=WAIT_BACKEND)
def g_enter_user_credentials_in_login_modal(selenium, browser_id, user,
                                            users, modals):
    driver = selenium[browser_id]
    _enter_user_credentials_in_login_modal(driver, modals, users[user])


@given(parsers.parse('user of {browser_id} seen {zone_name} '
                     'zone name in login page'))
@repeat_failed(timeout=WAIT_BACKEND)
def g_assert_zone_name_in_login_page(selenium, browser_id, zone_name):
    driver = selenium[browser_id]
    name = find_web_elem(driver, '.zone-company-name', 'no zone name found').text
    assert name.lower() == zone_name.lower(), \
        'found {} zone name instead of expected {}'.format(name, zone_name)
