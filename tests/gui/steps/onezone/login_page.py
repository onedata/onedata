"""This module contains gherkin steps to run acceptance tests featuring
login to onezone web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


from pytest_bdd import given, parsers, then, when

from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed, parse_seq


def _login_to_zone(zone_login_page, username, password):
    zone_login_page.username = username
    zone_login_page.password = password
    zone_login_page.sign_in()


@given(parsers.re('users? of (?P<browser_id_list>.*) logged '
                  'as (?P<user_id_list>.*) to Onezone service'))
@repeat_failed(timeout=WAIT_FRONTEND)
def g_login_to_zone_using_basic_auth(selenium, browser_id_list, user_id_list,
                                     oz_login_page, users):
    for browser_id, username in zip(parse_seq(browser_id_list),
                                    parse_seq(user_id_list)):
        _login_to_zone(oz_login_page(selenium[browser_id]), username,
                       users[username].password)


@when(parsers.re('users? of (?P<browser_id_list>.*) logs? '
                 'as (?P<user_id_list>.*) to Onezone service'))
@then(parsers.re('users? of (?P<browser_id_list>.*) logs? '
                 'as (?P<user_id_list>.*) to Onezone service'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_login_to_zone_using_basic_auth(selenium, browser_id_list, user_id_list,
                                      oz_login_page, users):
    for browser_id, username in zip(parse_seq(browser_id_list),
                                    parse_seq(user_id_list)):
        _login_to_zone(oz_login_page(selenium[browser_id]), username,
                       users[username].password)


@when(parsers.re(r'user of (?P<browser_id>.*?) types "(?P<text>.*?)" to '
                 r'(?P<in_box>Username|Password) input in Onezone login form'))
@then(parsers.re(r'user of (?P<browser_id>.*?) types "(?P<text>.*?)" to '
                 r'(?P<in_box>Username|Password) input in Onezone login form'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_enter_text_to_field_in_login_oz_form(selenium, browser_id, in_box,
                                            text, oz_login_page):
    setattr(oz_login_page(selenium[browser_id]), in_box.lower(), text)


@when(parsers.parse('user of {browser_id} clicks on the Sign in '
                    'button in oz login page'))
@then(parsers.parse('user of {browser_id} clicks on the Sign in '
                    'button in oz login page'))
@repeat_failed(timeout=WAIT_BACKEND)
def wt_click_on_sign_in_btn_in_oz_login_page(selenium, browser_id,
                                             oz_login_page):
    oz_login_page(selenium[browser_id]).sign_in()


@given(parsers.parse('user of {browser_id} seen {expected_name} '
                     'zone name in oz login page'))
@repeat_failed(timeout=WAIT_BACKEND)
def g_assert_zone_name_in_login_page(selenium, browser_id,
                                     expected_name, oz_login_page):
    displayed_name = oz_login_page(selenium[browser_id]).zone_name
    assert displayed_name == expected_name, \
        'found {} zone name instead of expected {}'.format(displayed_name,
                                                           expected_name)
