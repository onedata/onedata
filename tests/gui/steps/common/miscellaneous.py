"""This module contains gherkin steps to run acceptance tests in web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")

import time

from pytest_bdd import when, then, parsers

from selenium.webdriver.common.keys import Keys

from tests.gui.utils.generic import repeat_failed, transform
from tests.gui.conftest import WAIT_FRONTEND


@repeat_failed(attempts=WAIT_FRONTEND)
def _enter_text(input_box, text):
    input_box.clear()
    input_box.send_keys(text)
    if input_box.get_attribute('value') != text:
        raise RuntimeError('entering "{}" to input box failed'.format(text))


@when(parsers.parse('user of {browser_id} types "{text}" on keyboard'))
@then(parsers.parse('user of {browser_id} types "{text}" on keyboard'))
def type_string_into_active_element(selenium, browser_id, text):
    _enter_text(selenium[browser_id].switch_to.active_element, text)


@when(parsers.parse('user of {browser_id} types received '
                    '{item_type} on keyboard'))
@then(parsers.parse('user of {browser_id} types received '
                    '{item_type} on keyboard'))
def type_item_into_active_element(selenium, browser_id, item_type,
                                  tmp_memory):
    item = tmp_memory[browser_id]['mailbox'][item_type]
    _enter_text(selenium[browser_id].switch_to.active_element, item)


@when(parsers.parse('user of {browser_id} presses enter on keyboard'))
@then(parsers.parse('user of {browser_id} presses enter on keyboard'))
def press_enter_on_active_element(selenium, browser_id):
    driver = selenium[browser_id]
    driver.switch_to.active_element.send_keys(Keys.RETURN)


@when(parsers.re('user of (?P<browser_id>.+?) is idle for '
                 '(?P<seconds>\d*\.?\d+([eE][-+]?\d+)?) seconds'))
@then(parsers.re('user of (?P<browser_id>.+?) is idle for '
                 '(?P<seconds>\d*\.?\d+([eE][-+]?\d+)?) seconds'))
def wait_given_time(seconds):
    time.sleep(float(seconds))


@when(parsers.parse('user of {browser_id} should see that the page title '
                    'contains "{text}"'))
@then(parsers.parse('user of {browser_id} should see that the page title '
                    'contains "{text}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def title_contains(selenium, browser_id, text):
    page_title = selenium[browser_id].title
    assert page_title == text, \
        'page title is {} instead of expected {}'.format(page_title, text)


@when(parsers.parse('user of {browser_id} clicks on {btn} button '
                    'in {popup} popup'))
@then(parsers.parse('user of {browser_id} clicks on {btn} button '
                    'in {popup} popup'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_on_btn_in_popup(selenium, browser_id, btn, popup, popups):
    getattr(popups(selenium[browser_id]),
            transform(popup)).buttons[btn].click()
