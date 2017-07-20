"""This module contains gherkin steps to run acceptance tests featuring
login page in onepanel web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


from pytest_bdd import given, then, when, parsers

from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed, parse_seq, transform


def _login_to_panel(panel_login_page, username, password):
    panel_login_page.username = username
    panel_login_page.password = password
    panel_login_page.sign_in()


@given(parsers.re('users? of (?P<browser_id_list>.*) logged '
                  'as (?P<user_id_list>.*) to Onepanel service'))
@repeat_failed(timeout=WAIT_FRONTEND)
def g_login_to_panel_using_basic_auth(selenium, browser_id_list, user_id_list,
                                      panel_login_page, users):
    for browser_id, username in zip(parse_seq(browser_id_list),
                                    parse_seq(user_id_list)):
        _login_to_panel(panel_login_page(selenium[browser_id]), username,
                        users[username].password)


@when(parsers.re('users? of (?P<browser_id_list>.*) logs? '
                 'as (?P<user_id_list>.*) to Onepanel service'))
@then(parsers.re('users? of (?P<browser_id_list>.*) logs? '
                 'as (?P<user_id_list>.*) to Onepanel service'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_login_to_panel_using_basic_auth(selenium, browser_id_list, user_id_list,
                                       panel_login_page, users):
    for browser_id, username in zip(parse_seq(browser_id_list),
                                    parse_seq(user_id_list)):
        _login_to_panel(panel_login_page(selenium[browser_id]), username,
                        users[username].password)


@when(parsers.re(r'user of (?P<browser_id>.*?) types "(?P<text>.*?)" to '
                 r'(?P<in_box>Username|Password) input in Onepanel login form'))
@then(parsers.re(r'user of (?P<browser_id>.*?) types "(?P<text>.*?)" to '
                 r'(?P<in_box>Username|Password) input in Onepanel login form'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_enter_text_to_field_in_panel_login_form(selenium, browser_id, in_box,
                                               text, panel_login_page):
    setattr(panel_login_page(selenium[browser_id]),
            transform(in_box), text)


@when(parsers.parse('user of {browser_id} presses Sign in button '
                    'in Onepanel login page'))
@then(parsers.parse('user of {browser_id} presses Sign in button '
                    'in Onepanel login page'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_press_sign_in_btn_on_panel_login_page(selenium, browser_id,
                                             panel_login_page):
    panel_login_page(selenium[browser_id]).sign_in()


@when(parsers.parse('user of {browser_id} sees that he successfully '
                    'logged in {service}'))
@then(parsers.parse('user of {browser_id} sees that he successfully '
                    'logged in {service}'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_assert_successful_login(selenium, browser_id, onepage, service):
    logged_in_service = onepage(selenium[browser_id]).service
    assert logged_in_service.lower() == service.lower(), \
        'logged in {} instead of {}'.format(logged_in_service, service)


@when(parsers.parse('user of {browser_id} sees that he was logged out '
                    'from Onepanel'))
@then(parsers.parse('user of {browser_id} sees that he was logged out '
                    'from Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_assert_panel_login_page(selenium, browser_id, panel_login_page):
    _ = panel_login_page(selenium[browser_id]).header


@when(parsers.parse('user of {browser_id} sees error message '
                    'about invalid credentials in Onepanel login page'))
@then(parsers.parse('user of {browser_id} sees error message '
                    'about invalid credentials in Onepanel login page'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_assert_err_msg_about_credentials(selenium, browser_id, panel_login_page):
    assert panel_login_page(selenium[browser_id]).err_msg, \
        'no err msg about invalid credentials found'
