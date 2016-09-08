"""Common steps used in various GUI testing scenarios
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
import time
import random

from selenium.common.exceptions import NoSuchElementException, StaleElementReferenceException
from tests.utils.acceptance_utils import list_parser
from tests.gui.utils.generic import parse_url
from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND
from pytest_bdd import given, when, then, parsers
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.wait import WebDriverWait as Wait

from ..utils.generic import enter_text
from pytest_selenium_multi.pytest_selenium_multi import select_browser


@given(parsers.re('users? opened browser window for (?P<browser_id_list>.*)'))
def create_instances_of_webdriver(selenium, driver,
                                  config_driver, browser_id_list):
    for browser_id in list_parser(browser_id_list):
        if browser_id in selenium:
            raise AttributeError('{:s} already in use'.format(browser_id))
        else:
            selenium[browser_id] = config_driver(driver.get_instance())


@given(parsers.parse('user of {browser_id} generates valid name string'))
def name_string():
    chars = 'QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm1234567890'
    return ''.join(random.sample(chars, 6))


@when(parsers.parse('user of {browser_id} should see that the page title '
                    'contains "{text}"'))
@then(parsers.parse('user of {browser_id} should see that the page title '
                    'contains "{text}"'))
def title_contains(selenium, browser_id, text):
    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until(
        EC.title_contains(text),
        message='seeing that page title contains {:s}'.format(text)
    )


@when(parsers.parse('user of {browser_id} types given name on keyboard'))
@then(parsers.parse('user of {browser_id} types given name on keyboard'))
def type_valid_name_string_into_active_element(selenium, browser_id,
                                               name_string):
    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until(
        lambda s: enter_text(s.switch_to.active_element, name_string),
        message='entering {:s} to input box'.format(name_string)
    )


@when(parsers.parse('user of {browser_id} types "{text}" on keyboard'))
@then(parsers.parse('user of {browser_id} types "{text}" on keyboard'))
def type_string_into_active_element(selenium, browser_id, text):
    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until(
        lambda s: enter_text(s.switch_to.active_element, text),
        message='entering {:s} to input box'.format(text)
    )


@when(parsers.parse('user of {browser_id} types given token on keyboard'))
@then(parsers.parse('user of {browser_id} types given token on keyboard'))
def type_string_into_active_element(selenium, browser_id, tmp_memory):
    driver = select_browser(selenium, browser_id)
    token = tmp_memory[browser_id]['token']
    Wait(driver, WAIT_FRONTEND).until(
        lambda s: enter_text(s.switch_to.active_element, token),
        message='entering {:s} to input box'.format(token)
    )


@when(parsers.parse('user of {browser_id} presses enter on keyboard'))
@then(parsers.parse('user of {browser_id} presses enter on keyboard'))
def press_enter_on_active_element(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    driver.switch_to.active_element.send_keys(Keys.RETURN)


@when(parsers.parse('user of {browser_id} presses backspace on keyboard'))
@then(parsers.parse('user of {browser_id} presses backspace on keyboard'))
def press_enter_on_active_element(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    driver.switch_to.active_element.send_keys(Keys.BACKSPACE)


@then(parsers.parse('user of {browser_id} should see {links_names} links'))
def link_with_text_present(selenium, browser_id, links_names):
    driver = select_browser(selenium, browser_id)
    for name in list_parser(links_names):
        assert driver.find_element_by_link_text(name)


@given(parsers.re('users? of (?P<browser_id_list>.*) clicks on the '
                  '"(?P<link_name>.*)" link'))
def g_click_on_link_with_text(selenium, browser_id_list, link_name):
    for browser_id in list_parser(browser_id_list):
        driver = select_browser(selenium, browser_id)
        driver.find_element_by_link_text(link_name).click()


@when(parsers.parse('user of {browser_id} is idle for {seconds:d} seconds'))
def wait_n_seconds(seconds, browser_id):
    time.sleep(seconds)


@when(parsers.re(r'user of (?P<browser_id>.+) changes the relative URL to (?P<path>.+)'))
def visit_relative(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    driver.get(parse_url(driver.current_url).group('base_url') + path)


@then(parsers.parse('user of {browser_id} should see a page with "{text}" header'))
def page_with_header(selenium, browser_id, text):

    def header_with_text_presence(s):
        headers = s.find_elements_by_css_selector('h1, h2, h3, h4, h5')
        try:
            return any(map(lambda h: h.text == text, headers))
        except StaleElementReferenceException:
            return False

    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_BACKEND).until(header_with_text_presence)


@when(parsers.parse('user of {browser_id} sees an {notify_type} notify '
                    'with text matching to: {text_regexp}'))
@then(parsers.parse('user of {browser_id} sees an {notify_type} notify '
                    'with text matching to: {text_regexp}'))
def notify_visible_with_text(selenium, browser_id, notify_type, text_regexp):
    text_regexp = re.compile(text_regexp)

    def notify_with_text_present(s):
        try:
            notifiers = s.find_elements_by_css_selector(
                '.ember-notify.ember-notify-show.{t} .message'.format(t=notify_type)
            )
            if len(notifiers) > 0:
                matching_elements = [e for e in notifiers if text_regexp.match(e.text)]
                return len(matching_elements) > 0
            else:
                return None
        except NoSuchElementException:
            return None

    driver = select_browser(selenium, browser_id)
    Wait(driver, 2*WAIT_BACKEND).until(notify_with_text_present)


@when(parsers.parse('user of {browser_id} can see current url'))
def get_current_url(selenium, browser_id, tmp_memory):
    driver = select_browser(selenium, browser_id)
    url = driver.current_url
    if browser_id in tmp_memory:
        tmp_memory[browser_id]['url'] = url
    else:
        tmp_memory[browser_id] = {'url': driver.current_url}


@then(parsers.parse('user of {browser_id} sees that url has changed'))
def check_if_url_changed(selenium, browser_id, tmp_memory):
    driver = select_browser(selenium, browser_id)
    assert driver.current_url != tmp_memory[browser_id]['url']


@when('user of {browser_id} refreshes site')
@then('user of {browser_id} refreshes site')
def refresh_site(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    driver.refresh()


def select_button_from_buttons_by_name(name, buttons_selector):
    def _go_to_button(s):
        buttons = s.find_elements_by_css_selector(buttons_selector)
        for button in buttons:
            if button.text.lower() == name.lower():
                Wait(s, WAIT_FRONTEND).until(
                    EC.visibility_of(button)
                )
                if button.is_enabled():
                    return button
    return _go_to_button


def find_element_by_css_selector_and_text(selector, text):
    """finds element on site by css selector and element's text"""
    def _find_element(s):
        elements_list = s.find_elements_by_css_selector(selector)
        for elem in elements_list:
            if elem.text.lower() == text.lower():
                return elem
    return _find_element


# Below functions are currently unused and should not be used,
# because it involves a knowledge about internals...


@when(parsers.re(r'user changes application path to (?P<path>.+)'))
def on_ember_path(selenium, path):
    selenium.get(parse_url(selenium.current_url).group('base_url') + '/#' + path)
