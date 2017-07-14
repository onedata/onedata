"""Steps for Login page Onezone.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from pytest_bdd import given, parsers, then, when

from tests.gui.conftest import WAIT_BACKEND
from tests.gui.utils.generic import repeat_failed, find_web_elem


def _enter_user_credentials_in_login_modal(login_page, user, users):
    login_page.username = user
    login_page.password = users[user].password


@when(parsers.parse('user of {browser_id} enters credentials of {user} '
                    'in login form in oz login page'))
@when(parsers.parse('user of {browser_id} enters credentials of {user} '
                    'in login form in oz login page'))
@repeat_failed(timeout=WAIT_BACKEND)
def wt_enter_user_credentials_in_login_modal(selenium, browser_id, user,
                                             users, oz_login_page):
    driver = selenium[browser_id]
    _enter_user_credentials_in_login_modal(oz_login_page(driver), user, users)


@given(parsers.parse('user of {browser_id} entered credentials of {user} '
                     'in login form in oz login page'))
@repeat_failed(timeout=WAIT_BACKEND)
def g_enter_user_credentials_in_login_modal(selenium, browser_id, user,
                                            users, oz_login_page):
    driver = selenium[browser_id]
    _enter_user_credentials_in_login_modal(oz_login_page(driver), user, users)


@given(parsers.parse('user of {browser_id} clicked on the "username" '
                     'login button in oz login page'))
@repeat_failed(timeout=WAIT_BACKEND)
def g_click_on_username_login_btn(selenium, browser_id, oz_login_page):
    driver = selenium[browser_id]
    oz_login_page(driver).username_login()


@given(parsers.parse('user of {browser_id} clicked on the Sign in '
                     'button in oz login page'))
@repeat_failed(timeout=WAIT_BACKEND)
def g_click_on_sign_in_btn_in_oz_login_page(selenium, browser_id,
                                            oz_login_page):
    driver = selenium[browser_id]
    oz_login_page(driver).sign_in()


@given(parsers.parse('user of {browser_id} seen {zone_name} '
                     'zone name in oz login page'))
@repeat_failed(timeout=WAIT_BACKEND)
def g_assert_zone_name_in_login_page(selenium, browser_id,
                                     zone_name, oz_login_page):
    login_page = oz_login_page(selenium[browser_id])
    displayed_name = login_page.zone_name
    assert displayed_name.lower() == zone_name.lower(), \
        'found {} zone name instead of expected {}'.format(displayed_name,
                                                           zone_name)
