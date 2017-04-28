"""Steps for Login page Onezone.
"""

from pytest_bdd import given, parsers, then, when
from pytest_selenium_multi.pytest_selenium_multi import select_browser

from tests.gui.conftest import WAIT_BACKEND
from tests.gui.utils.generic import repeat_failed

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


def _enter_user_credentials_in_login_modal(driver, modals, user, users):
    modal = modals(driver).login
    modal.username = user
    modal.password = users[user]


@when(parsers.parse('user of {browser_id} enters credentials of {user} '
                    'in "Login with username and password" modal'))
@then(parsers.parse('user of {browser_id} enters credentials of {user} '
                    'in "Login with username and password" modal'))
@repeat_failed(timeout=WAIT_BACKEND)
def wt_enter_user_credentials_in_login_modal(selenium, browser_id, user,
                                             users, modals):
    driver = select_browser(selenium, browser_id)
    _enter_user_credentials_in_login_modal(driver, modals, user, users)


@given(parsers.parse('user of {browser_id} entered credentials of {user} '
                     'in "Login with username and password" modal'))
@repeat_failed(timeout=WAIT_BACKEND)
def g_enter_user_credentials_in_login_modal(selenium, browser_id, user,
                                            users, modals):
    driver = select_browser(selenium, browser_id)
    _enter_user_credentials_in_login_modal(driver, modals, user, users)
