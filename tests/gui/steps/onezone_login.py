"""Steps for features of Onezone login page.
"""
from tests.utils.cucumber_utils import list_parser

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
from pytest_bdd import given, then
from pytest_bdd import parsers

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


@then(parsers.parse('I should see login buttons for {provider_names}'))
def find_provider_button(selenium, provider_names):
    for name in list_parser(provider_names):
        assert selenium.find_element_by_css_selector(
            'a.login-icon-box.{name}'.format(name=name)
        )
