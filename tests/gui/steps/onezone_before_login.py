"""Steps for features of Onezone login page.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
from selenium.common.exceptions import TimeoutException
from tests.utils.cucumber_utils import list_parser
from selenium.webdriver.support.ui import WebDriverWait as wait
from pytest_bdd import given, when, then, parsers
from tests.gui.utils.generic import parse_url, go_to_relative_url


@given("user opens a Onezone URL in a web browser")
def visit_onezone(base_url, selenium):
    oz_url = base_url
    selenium.get(oz_url)


# @given("I'm logged in to Onezone")
# def logged_in_to_onezone(selenium):
#     """Will check if going to / will redirect to onezone page (default when logged in)
#     If not - try to login with dev_login"""
#     go_to_relative_url(selenium, '/')
#     try:
#         wait(selenium, 2).\
#             until(lambda s: parse_url(s.current_url).group('method') == '/onezone')
#     except TimeoutException:
#         go_to_relative_url(selenium, '/dev_login')
#         selenium.find_element_by_css_selector('a').click()
#         wait(selenium, 4). \
#             until(lambda s: parse_url(s.current_url).group('method') == '/onezone',
#                   'Current URL method: {m}'.format(m=parse_url(selenium.current_url).group('method')))


@then(parsers.parse('user should see login buttons for {provider_names}'))
def login_provider_buttons(selenium, provider_names):
    for name in list_parser(provider_names):
        assert selenium.find_element_by_css_selector(
            '.login-box a.login-icon-box.{name}'.format(name=name)
        )


def _click_login_provider_button(selenium, provider_name):
    selenium.find_element_by_css_selector(
        '.login-box a.login-icon-box.{name}'.format(name=provider_name)).click()


@given(parsers.parse('user clicks on the "{provider_name}" login button'))
def g_click_login_provider_button(selenium, provider_name):
    _click_login_provider_button(selenium, provider_name)


@when(parsers.parse('user clicks on the "{provider_name}" login button'))
def w_click_login_provider_button(selenium, provider_name):
    _click_login_provider_button(selenium, provider_name)







@then(parsers.re('I should be redirected to (?P<page>.+) page'))
def being_redirected_to_page(page, selenium):
    wait(selenium, 5).until(lambda s: re.match(r'https?://.*?(/#)?(/.*)', s.current_url).group(2) == page)


# @when('I login with development login link as "{user}"')
# def login_dev_onezone_with_url(selenium, base_url, user):
#     url = '{oz_url}/validate_dev_login?user={user}'.format(oz_url=base_url, user=user)
#     url.replace('//', '/')
#     selenium.get(url)
