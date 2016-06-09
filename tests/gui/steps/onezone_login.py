"""Steps for features of Onezone login page.
"""
from tests.utils.cucumber_utils import list_parser

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
from pytest_bdd import given, then
from pytest_bdd import parsers
from selenium.webdriver.support.ui import WebDriverWait as wait

from pytest_bdd import given, when, then, parsers
import re


@given("I'm visiting Onezone site")
def visit_onezone(base_url, selenium):
    oz_url = base_url
    selenium.get(oz_url)


@when('I go to the <page> page')
@when(parsers.re(r'I go to the (?P<page>.+) page'))
def visit_login_page(selenium, page):
    selenium.get(selenium.current_url + '#' + page)


@then('The page title should contain <title>')
@then('The page title should contain {title}')
def title_matches(selenium, title):
    assert re.match(r'.*' + title + r'.*', selenium.title, re.IGNORECASE)


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
