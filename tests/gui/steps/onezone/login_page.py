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


def _enter_user_credentials_in_login_modal(driver, modals, user, tmp_memory):
    modal = modals(driver).login
    user_cred = tmp_memory['users'][user]
    modal.username = user_cred.name
    modal.password = user_cred.password


@when(parsers.parse('user of {browser_id} enters credentials of {user} '
                    'in "Login with username and password" modal'))
@then(parsers.parse('user of {browser_id} enters credentials of {user} '
                    'in "Login with username and password" modal'))
@repeat_failed(timeout=WAIT_BACKEND)
def wt_enter_user_credentials_in_login_modal(selenium, browser_id, user,
                                             tmp_memory, modals):
    driver = select_browser(selenium, browser_id)
    _enter_user_credentials_in_login_modal(driver, modals, user, tmp_memory)


@given(parsers.parse('user of {browser_id} entered credentials of {user} '
                     'in "Login with username and password" modal'))
@repeat_failed(timeout=WAIT_BACKEND)
def g_enter_user_credentials_in_login_modal(selenium, browser_id, user,
                                            tmp_memory, modals):
    driver = select_browser(selenium, browser_id)
    _enter_user_credentials_in_login_modal(driver, modals, user, tmp_memory)
