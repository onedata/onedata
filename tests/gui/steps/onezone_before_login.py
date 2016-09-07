"""Steps for features of Onezone login page.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
from selenium.webdriver.support.ui import WebDriverWait as wait
from pytest_bdd import given, when, then, parsers

from pytest_selenium_multi.pytest_selenium_multi import select_browser


def _open_onezone_url(driver, base_url):
    oz_url = base_url
    driver.get(oz_url)


@given(parsers.parse("user of {browser_id} opens a Onezone URL"))
def g_visit_onezone(base_url, selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    _open_onezone_url(driver, base_url)


@when(parsers.parse("user of {browser_id} opens a Onezone URL"))
def w_visit_onezone(base_url, selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    _open_onezone_url(driver, base_url)


@then(parsers.parse('user of {browser_id} should see login button '
                    'for {provider_name}'))
def login_provider_buttons(selenium, browser_id, provider_name):
    driver = select_browser(selenium, browser_id)
    assert driver.find_element_by_css_selector(
            '.login-box a.login-icon-box.{name}'.format(name=provider_name)
        )


def _click_login_provider_button(driver, provider_name):
    driver.find_element_by_css_selector(
        '.login-box a.login-icon-box.{:s}'.format(provider_name)).click()


@given(parsers.parse('user of {browser_id} clicks on the "{provider_name}" '
                     'login button'))
def g_click_login_provider_button(selenium, browser_id, provider_name):
    driver = select_browser(selenium, browser_id)
    _click_login_provider_button(driver, provider_name)


@when(parsers.parse('user of {browser_id} clicks on the "{provider_name}" '
                    'login button'))
def w_click_login_provider_button(selenium, browser_id, provider_name):
    driver = select_browser(selenium, browser_id)
    _click_login_provider_button(driver, provider_name)


@then(parsers.re('user of (?P<browser_id>.+) should be '
                 'redirected to (?P<page>.+) page'))
def being_redirected_to_page(page, selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    wait(driver, 5).until(lambda s: re.match(r'https?://.*?(/#)?(/.*)', s.current_url).group(2) == page)

