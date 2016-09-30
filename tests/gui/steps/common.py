"""Common steps used in various GUI testing scenarios
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
import time
import random
import pyperclip

from selenium.common.exceptions import NoSuchElementException, StaleElementReferenceException
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.wait import WebDriverWait as Wait
from selenium.webdriver.support.expected_conditions import staleness_of

from tests.utils.acceptance_utils import list_parser
from tests.gui.utils.generic import parse_url, enter_text
from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND

from pytest_bdd import given, when, then, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser


@given(parsers.parse("user opened {browser_id_list} window"))
@given(parsers.parse("users opened {browser_id_list} browsers' windows"))
def create_instances_of_webdriver(selenium, driver,
                                  config_driver, browser_id_list,
                                  tmp_memory):
    for browser_id in list_parser(browser_id_list):
        if browser_id in selenium:
            raise AttributeError('{:s} already in use'.format(browser_id))
        else:
            selenium[browser_id] = config_driver(driver.get_instance())
            tmp_memory[browser_id] = {'shares': {},
                                      'spaces': {},
                                      'groups': {},
                                      'mailbox': {},
                                      'window': {'modal': None}}


@given(parsers.parse('user of {browser_id} generates valid name string'))
def name_string(tmp_memory, browser_id):
    chars = 'QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm1234567890'
    gen_str = ''.join(random.sample(chars, 6))
    tmp_memory[browser_id]['gen_str'] = gen_str
    return gen_str


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


@when(parsers.parse('user of {browser_id} types received '
                    '{item_type} on keyboard'))
@then(parsers.parse('user of {browser_id} types received '
                    '{item_type} on keyboard'))
def type_item_into_active_element(selenium, browser_id, item_type,
                                  tmp_memory):
    driver = select_browser(selenium, browser_id)
    item = tmp_memory[browser_id]['mailbox'][item_type]
    Wait(driver, WAIT_FRONTEND).until(
        lambda s: enter_text(s.switch_to.active_element, item),
        message='entering {:s} to input box'.format(item)
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


@given(parsers.re('users? of (?P<browser_id_list>.*) clicked on the '
                  '"(?P<link_name>.*)" link'))
def g_click_on_link_with_text(selenium, browser_id_list, link_name):
    for browser_id in list_parser(browser_id_list):
        driver = select_browser(selenium, browser_id)
        driver.find_element_by_link_text(link_name).click()


@when(parsers.parse('user of {browser_id} is idle for {seconds:d} seconds'))
def wait_n_seconds(seconds):
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


@when(parsers.parse('user of {browser_id} closes all notifies'))
@then(parsers.parse('user of {browser_id} closes all notifies'))
def close_notifies(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    notifies = driver.find_elements_by_css_selector('.ember-notify '
                                                    'a.close-button, '
                                                    '.ember-notify '
                                                    '.message')
    for cls_btn, msg in zip(notifies[::2], notifies[1::2]):
        cls_btn.click()
        Wait(driver, WAIT_BACKEND).until(
            staleness_of(cls_btn),
            message='waiting for notify "{:s}" to disappear'.format(msg)
        )


@when(parsers.parse('user of {browser_id} refreshes site'))
@then(parsers.parse('user of {browser_id} refreshes site'))
def refresh_site(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    driver.refresh()


@when(parsers.parse('user of {browser_id} sees that url matches {path}'))
@then(parsers.parse('user of {browser_id} sees that url matches {path}'))
def is_url_matching(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    assert re.search(path, driver.current_url)


@when(parsers.parse('user of {browser_id} opens received url'))
@then(parsers.parse('user of {browser_id} opens received url'))
def open_received_url(selenium, browser_id, tmp_memory, base_url):
    driver = select_browser(selenium, browser_id)

    old_page = driver.find_element_by_css_selector('html')
    url = tmp_memory[browser_id]['mailbox']['url']
    driver.get(url.replace(parse_url(url).group('base_url'), base_url, 1))

    Wait(driver, WAIT_BACKEND).until(
        staleness_of(old_page),
        message='waiting for page {:s} to load'.format(url)
    )


@when(parsers.re('user of (?P<browser_id>.*?) sends copied (?P<item_type>.*?) '
                 'to users? of (?P<browser_list>.*)'))
@then(parsers.re('user of (?P<browser_id>.*?) sends copied (?P<item_type>.*?) '
                 'to users? of (?P<browser_list>.*)'))
def send_copied_item_to_other_users(item_type, browser_list, tmp_memory):
    item = pyperclip.paste()
    for browser in list_parser(browser_list):
        tmp_memory[browser]['mailbox'][item_type] = item


# Below functions are currently unused and should not be used,
# because it involves a knowledge about internals...


@when(parsers.re(r'user of (?P<browser_id>.*?) changes '
                 r'application path to plain (?P<path>.+)'))
@then(parsers.re(r'user of (?P<browser_id>.*?) changes '
                 r'application path to plain (?P<path>.+)'))
def on_ember_path(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    driver.get(parse_url(driver.current_url).group('base_url') + '/#' + path)


@when(parsers.re(r'user of (?P<browser_id>.*?) changes application path to '
                 r'(?P<path>.+?) concatenated with copied item'))
@then(parsers.re(r'user of (?P<browser_id>.*?) changes application path to '
                 r'(?P<path>.+?) concatenated with copied item'))
def change_app_path_with_copied_item(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    base_url = parse_url(driver.current_url).group('base_url')
    url = '{base_url}/#/{path}/{item}'.format(base_url=base_url,
                                              path=path,
                                              item=pyperclip.paste())
    driver.get(url)


@when(parsers.re(r'user of (?P<browser_id>.*?) changes application path to '
                 r'(?P<path>.+?) concatenated with received (?P<item>.*)'))
@then(parsers.re(r'user of (?P<browser_id>.*?) changes application path to '
                 r'(?P<path>.+?) concatenated with received (?P<item>.*)'))
def change_app_path_with_recv_item(selenium, browser_id, path,
                                   tmp_memory, item):
    driver = select_browser(selenium, browser_id)
    base_url = parse_url(driver.current_url).group('base_url')
    item = tmp_memory[browser_id]['mailbox'][item]
    url = '{base_url}/#/{path}/{item}'.format(base_url=base_url,
                                              path=path,
                                              item=item)
    driver.get(url)
