"""Common steps used in various GUI testing scenarios
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
import time
import random
import stat
import subprocess

import pyperclip

from selenium.common.exceptions import NoSuchElementException, StaleElementReferenceException
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.wait import WebDriverWait as Wait
from selenium.webdriver.support.expected_conditions import staleness_of

from tests.gui.utils.generic import parse_seq
from tests.gui.utils.generic import parse_url, enter_text
from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND, set_global_browser_being_created, \
    is_firefox_logging_enabled

from pytest_bdd import given, when, then, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser


PERMS_777 = stat.S_IRWXU | stat.S_IRWXG | stat.S_IROTH | stat.S_IXOTH


@given(parsers.parse("user opened {browser_id_list} window"))
@given(parsers.parse("users opened {browser_id_list} browsers' windows"))
def create_instances_of_webdriver(selenium, driver,
                                  config_driver, browser_id_list,
                                  tmp_memory, tmpdir):
    for browser_id in parse_seq(browser_id_list):
        if browser_id in selenium:
            raise AttributeError('{:s} already in use'.format(browser_id))
        else:
            set_global_browser_being_created(browser_id)
            selenium[browser_id] = config_driver(driver.get_instance())

            tmp_memory[browser_id] = {'shares': {},
                                      'spaces': {},
                                      'groups': {},
                                      'mailbox': {},
                                      'oz': {},
                                      'window': {'modal': None}}

            selenium[browser_id].instance_name = browser_id
            selenium[browser_id].ff_logs_enabled = is_firefox_logging_enabled
            selenium[browser_id].root_dir = str(tmpdir)


@given(parsers.parse('user of {browser_id} generates valid name string'))
def name_string(tmp_memory, browser_id):
    chars = 'QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm1234567890'
    gen_str = ''.join(random.sample(chars, 6))
    tmp_memory[browser_id]['gen_str'] = 'g_' + gen_str
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
    for name in parse_seq(links_names):
        assert driver.find_element_by_link_text(name), \
            '{} link not found'.format(name)


@given(parsers.re('users? of (?P<browser_id_list>.*) clicked on the '
                  '"(?P<link_name>.*)" link'))
def g_click_on_link_with_text(selenium, browser_id_list, link_name):
    for browser_id in parse_seq(browser_id_list):
        driver = select_browser(selenium, browser_id)
        driver.find_element_by_link_text(link_name).click()


@when(parsers.re('users? of (?P<browser_id_list>.*) clicks on the '
                 '"(?P<link_name>.*)" link'))
@then(parsers.re('users? of (?P<browser_id_list>.*) clicks on the '
                 '"(?P<link_name>.*)" link'))
def wt_click_on_link_with_text(selenium, browser_id_list, link_name):
    for browser_id in parse_seq(browser_id_list):
        driver = select_browser(selenium, browser_id)
        driver.find_element_by_link_text(link_name).click()


@when(parsers.re('user of (?P<browser_id>.+?) is idle for '
                 '(?P<seconds>\d*\.?\d+([eE][-+]?\d+)?) seconds'))
def wait_n_seconds(seconds):
    time.sleep(float(seconds))


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
        # TODO currently we ignore stalement because some old notify may have disappeared
        #  and probability of new notify to disappear is small
        except (NoSuchElementException, StaleElementReferenceException):
            return None

    driver = select_browser(selenium, browser_id)
    Wait(driver, 2*WAIT_BACKEND).until(
        notify_with_text_present,
        message='waiting for notify matching: {}'.format(text_regexp.pattern)
    )


@when(parsers.parse('user of {browser_id} closes all notifies'))
@then(parsers.parse('user of {browser_id} closes all notifies'))
def close_notifies(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    notify = driver.find_elements_by_css_selector('.ember-notify '
                                                  'a.close-button, '
                                                  '.ember-notify '
                                                  '.message')
    notify = notify[:2] if notify else None

    while notify:
        cls_btn, msg = notify
        try:
            msg = msg.text
            cls_btn.click()
            Wait(driver, WAIT_BACKEND).until(
                staleness_of(cls_btn),
                message='waiting for notify "{:s}" to disappear'.format(msg)
            )
        except StaleElementReferenceException:
            pass

        notify = driver.find_elements_by_css_selector('.ember-notify '
                                                      'a.close-button, '
                                                      '.ember-notify '
                                                      '.message')
        notify = notify[:2] if notify else None


@when(parsers.parse('user of {browser_id} refreshes site'))
@then(parsers.parse('user of {browser_id} refreshes site'))
def refresh_site(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    driver.refresh()


@when(parsers.parse('user of {browser_id} refreshes webapp'))
@then(parsers.parse('user of {browser_id} refreshes webapp'))
def refresh_site(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    driver.get(parse_url(driver.current_url).group('base_url'))


@when(parsers.re('user of (?P<browser_id>.+?) sees that '
                 '(?:url|URL) matches (?P<path>.+?)'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that '
                 '(?:url|URL) matches (?P<path>.+?)'))
def is_url_matching(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    assert re.search(path, driver.current_url), \
        '{} url is not like expected {}'.format(driver.current_url, path)


@when(parsers.re('user of (?P<browser_id>.+?) opens received (?:url|URL)'))
@then(parsers.re('user of (?P<browser_id>.+?) opens received (?:url|URL)'))
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
    for browser in parse_seq(browser_list):
        tmp_memory[browser]['mailbox'][item_type.lower()] = item


# Below functions are currently unused and should not be used,
# because it involves a knowledge about internals...


@when(parsers.re(r'user of (?P<browser_id>.*?) changes '
                 r'application path to plain (?P<path>.+)'))
@then(parsers.re(r'user of (?P<browser_id>.*?) changes '
                 r'application path to plain (?P<path>.+)'))
def on_ember_path(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    driver.get(parse_url(driver.current_url).group('base_url') + '/#' + path)


@when(parsers.re(r'user of (?P<browser_id>.*?) changes webapp path to '
                 r'(?P<path>.+?) concatenated with copied item'))
@then(parsers.re(r'user of (?P<browser_id>.*?) changes webapp path to '
                 r'(?P<path>.+?) concatenated with copied item'))
def change_app_path_with_copied_item(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    base_url = parse_url(driver.current_url).group('base_url')
    url = '{base_url}{path}/{item}'.format(base_url=base_url,
                                           path=path,
                                           item=pyperclip.paste())
    driver.get(url)


@when(parsers.re(r'user of (?P<browser_id>.*?) changes webapp path to '
                 r'(?P<path>.+?) concatenated with received (?P<item>.*)'))
@then(parsers.re(r'user of (?P<browser_id>.*?) changes webapp path to '
                 r'(?P<path>.+?) concatenated with received (?P<item>.*)'))
def change_app_path_with_recv_item(selenium, browser_id, path,
                                   tmp_memory, item):
    driver = select_browser(selenium, browser_id)
    base_url = parse_url(driver.current_url).group('base_url')
    item = tmp_memory[browser_id]['mailbox'][item.lower()]
    url = '{base_url}{path}/{item}'.format(base_url=base_url,
                                           path=path,
                                           item=item)
    driver.get(url)


def _create_dir(root, dir_path, perms=PERMS_777):
    dir_created = root.mkdir(*dir_path)
    dir_created.chmod(perms)
    return dir_created


def _create_temp_dir(root, path, recursive=True):
    directory = path

    if recursive:
        for directory in path[:-1]:
            next_dir = root.join(directory)
            if next_dir.isdir():
                root = next_dir
            else:
                root = _create_dir(root, [directory])
        else:
            directory = [path[-1]]

    directory = _create_dir(root, directory)
    return directory


def _create_temp_file(directory, file_name, file_content='', perms=PERMS_777):
    reg_file = directory.join(file_name)
    reg_file.write(file_content)
    reg_file.chmod(perms)
    return reg_file


@given(parsers.parse('user of {browser_id} has {num:d} files '
                     'in directory named "{dir_path}"'))
def create_temp_dir_with_files(browser_id, num, dir_path, tmpdir):
    path = [browser_id]
    path.extend(dir_path.split('/'))
    directory = _create_temp_dir(tmpdir, path, recursive=True)
    for i in range(10, num + 10):
        _create_temp_file(directory, 'file_{}.txt'.format(i), '1' * i)


@when(parsers.parse('user of browser sees that copied token matches displayed one'))
@then(parsers.parse('user of browser sees that copied token matches displayed one'))
def assert_copied_token_match_displayed_one(browser_id, tmp_memory):
    displayed_token = tmp_memory[browser_id]['token']
    copied_token = pyperclip.paste()
    err_msg = 'Displayed token: {} does not match copied one: ' \
              '{}'.format(displayed_token, copied_token)
    assert copied_token == displayed_token, err_msg


@when(parsers.parse('user of browser sees that copied token '
                    'does not match displayed one'))
@then(parsers.parse('user of browser sees that copied token '
                    'does not match displayed one'))
def assert_copied_token_does_not_match_displayed_one(browser_id, tmp_memory):
    displayed_token = tmp_memory[browser_id]['token']
    copied_token = pyperclip.paste()
    err_msg = 'Displayed token: {} match copied one: {} ' \
              'while it should not be'.format(displayed_token, copied_token)
    assert copied_token != displayed_token, err_msg


@given(parsers.parse('there are no working provider(s) named {provider_list}'))
def kill_providers(persistent_environment, provider_list):
    for provider in parse_seq(provider_list):
        regexp = r'worker@(.*?{name}.*)'.format(name=provider)
        for node in persistent_environment["op_worker_nodes"]:
            container_name = re.match(regexp, node).groups(0)[0]
            subprocess.call(['docker', 'kill', container_name])
