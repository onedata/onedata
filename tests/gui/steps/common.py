"""Common steps used in various GUI testing scenarios
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re

from pytest_bdd import when, then, parsers
from selenium.webdriver.common.keys import Keys


@then('The page title should contain "<title>"')
@then(parsers.parse('The page title should contain "{title}"'))
def title_matches(selenium, title):
    assert re.match(r'.*' + title + r'.*', selenium.title, re.IGNORECASE)


@when('I type "<name>" into active element')
@when(parsers.parse('I type "{name}" into active element'))
def type_string_into_active_element(selenium, name):
    selenium.switch_to.active_element.send_keys(name)


@when(parsers.parse('I press enter on active element'))
def press_enter_on_active_element(selenium):
    selenium.switch_to.active_element.send_keys(Keys.RETURN)