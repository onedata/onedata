"""Common steps used in various GUI testing scenarios
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import time
from tests.gui.utils.generic import parse_url
from tests.gui.conftest import WAIT_FRONTEND
from pytest_bdd import given, when, then, parsers
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.wait import WebDriverWait as Wait

@when(parsers.parse('I wait {seconds:d} seconds'))
def wait_n_seconds(seconds):
    time.sleep(seconds)


@when(parsers.re(r'I go to the (?P<path>.+) relative URL'))
def visit_relative(selenium, path):
    selenium.get(parse_url(selenium.current_url).group('base_url') + path)


@given(parsers.re(r'I am on the (?P<path>.+) Ember path'))
def on_ember_path(selenium, path):
    selenium.get(parse_url(selenium.current_url).group('base_url') + '/#' + path)

@when(parsers.re(r'I go to the (?P<path>.+) Ember path'))
def visit_ember_path(selenium, path):
    selenium.get(parse_url(selenium.current_url).group('base_url') + '/#' + path)


@then('The page title should contain "<text>"')
@then(parsers.parse('The page title should contain "{text}"'))
def title_contains(selenium, text):
    Wait(selenium, WAIT_FRONTEND).until(EC.title_contains(text))


@when('I type "<name>" into active element')
@when(parsers.parse('I type "{name}" into active element'))
def type_string_into_active_element(selenium, name):
    selenium.switch_to.active_element.send_keys(name)


@when(parsers.parse('I press enter on active element'))
def press_enter_on_active_element(selenium):
    selenium.switch_to.active_element.send_keys(Keys.RETURN)
