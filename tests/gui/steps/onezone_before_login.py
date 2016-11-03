"""Steps for features of Onezone login page.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
from selenium.webdriver.support.ui import WebDriverWait as wait
from pytest_bdd import given, when, then, parsers
from tests.gui.utils.generic import parse_seq
from pytest_selenium_multi.pytest_selenium_multi import select_browser


@given(parsers.re("users? of (?P<browser_id_list>.*) opened Onezone URL"))
def g_visit_onezone(base_url, selenium, browser_id_list):
    for browser_id in parse_seq(browser_id_list):
        driver = select_browser(selenium, browser_id)
        oz_url = base_url
        driver.get(oz_url)


@then(parsers.parse('user of {browser_id} should see login button '
                    'for {provider_name}'))
def login_provider_buttons(selenium, browser_id, provider_name):
    driver = select_browser(selenium, browser_id)
    assert driver.find_element_by_css_selector(
            '.login-box a.login-icon-box.{name}'.format(name=provider_name)
        ), 'login for provider {} not found'.format(provider_name)


def _click_login_provider_button(driver, provider_name):
    driver.find_element_by_css_selector(
        '.login-box a.login-icon-box.{:s}'.format(provider_name)
    ).click()


@given(parsers.re('users? of (?P<browser_id_list>.*) clicked on the '
                  '"(?P<provider_name>.*)" login button'))
def g_click_login_provider_button(selenium, browser_id_list, provider_name):
    for browser_id in parse_seq(browser_id_list):
        driver = select_browser(selenium, browser_id)
        _click_login_provider_button(driver, provider_name)


@when(parsers.re('users? of (?P<browser_id_list>.*) clicks on the '
                 '"(?P<provider_name>.*)" login button'))
def w_click_login_provider_button(selenium, browser_id_list, provider_name):
    for browser_id in parse_seq(browser_id_list):
        driver = select_browser(selenium, browser_id)
        _click_login_provider_button(driver, provider_name)


@then(parsers.re('user of (?P<browser_id>.+) should be '
                 'redirected to (?P<page>.+) page'))
def being_redirected_to_page(page, selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    wait(driver, 5).until(lambda s: re.match(r'https?://.*?(/#)?(/.*)', s.current_url).group(2) == page)


@given(parsers.re('users? of (?P<browser_id_list>.*) logged '
                  'as (?P<user_id_list>.*)'))
def log_to_user_in_each_browser(selenium, browser_id_list,
                                user_id_list):
    for browser_id, user_id in zip(parse_seq(browser_id_list),
                                   parse_seq(user_id_list)):
        driver = select_browser(selenium, browser_id)
        driver.find_element_by_link_text(user_id).click()
