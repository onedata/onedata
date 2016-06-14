"""Steps for features of Onezone login page.
"""
from selenium.common.exceptions import TimeoutException

from tests.gui.conftest import SELENIUM_IMPLICIT_WAIT
from tests.utils.cucumber_utils import list_parser

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
import re
from selenium.webdriver.support.ui import WebDriverWait as wait
from pytest_bdd import given, when, then, parsers
from tests.gui.utils.generic import parse_url, go_to_relative_url


@given("I'm visiting Onezone site")
def visit_onezone(base_url, selenium):
    oz_url = base_url
    selenium.get(oz_url)


@given("I'm logged in to Onezone")
def logged_in_to_onezone(base_url, selenium):
    """Will check if going to / will redirect to onezone page (default when logged in)
    If not - try to login with dev_login"""
    go_to_relative_url(selenium, '/')
    try:
        wait(selenium, 2).\
            until(lambda s: parse_url(s.current_url).group('method') == '/#/onezone')
    except TimeoutException:
        go_to_relative_url(selenium, '/dev_login')
        selenium.find_element_by_css_selector('a').click()
        wait(selenium, 4). \
            until(lambda s: parse_url(s.current_url).group('method') == '/#/onezone')


@when('I click on the first development login button')
def click_first_dev_login(selenium):
    btn = selenium.find_element_by_css_selector('a')
    btn.click()


@then(parsers.parse('I should see at least {btn_count:d} login buttons'))
def find_n_login_buttons(selenium, btn_count):
    assert len(selenium.find_elements_by_css_selector('a.login-icon-box')) >= btn_count


@given(parsers.parse('A login button for {provider_name}'))
def login_button(provider_name, selenium):
    return selenium.find_element_by_css_selector(
        'a.login-icon-box.{name}'.format(name=provider_name)
    )


@then(parsers.parse('I should see login buttons for {provider_names}'))
def login_provider_buttons(selenium, provider_names):
    for name in list_parser(provider_names):
        assert selenium.find_element_by_css_selector(
            'a.login-icon-box.{name}'.format(name=name)
        )


@when('I click on the login button')
def click_login_provider_button(login_button):
    login_button.click()


# TODO: move to gui test utils
@then(parsers.re('I should be redirected to (?P<page>.+) page'))
def being_redirected_to_page(page, selenium):
    wait(selenium, 5).until(lambda s: re.match(r'https?://.*?(/#)?(/.*)', s.current_url).group(2) == page)


@then('I should see a development login page with at least 1 validate login link')
def see_development_login_page(selenium):
    assert re.match(r'.*/validate_dev_login.*',
                    selenium.find_element_by_css_selector('a').get_attribute('href'))


@when('I login with development login link as "{user}"')
def login_dev_onezone_with_url(selenium, base_url, user):
    url = '{oz_url}/validate_dev_login?user={user}'.format(oz_url=base_url, user=user)
    url.replace('//', '/')
    selenium.get(url)
