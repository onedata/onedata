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

from selenium.common.exceptions import NoSuchElementException, StaleElementReferenceException
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.wait import WebDriverWait as Wait
from selenium.webdriver.support.expected_conditions import staleness_of

from tests.gui.utils.generic import parse_seq, suppress, repeat_failed
from tests.gui.utils.generic import parse_url, enter_text, redirect_display
from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND

from pytest_bdd import given, when, then, parsers


PERMS_777 = stat.S_IRWXU | stat.S_IRWXG | stat.S_IROTH | stat.S_IXOTH


@given(parsers.parse('user of {browser_id} generates valid name string'))
def name_string(tmp_memory, browser_id):
    chars = 'QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm1234567890'
    gen_str = 'g_' + ''.join(random.sample(chars, 6))
    tmp_memory[browser_id]['gen_str'] = gen_str
    return gen_str


@when(parsers.parse('user of {browser_id} should see that the page title '
                    'contains "{text}"'))
@then(parsers.parse('user of {browser_id} should see that the page title '
                    'contains "{text}"'))
def title_contains(selenium, browser_id, text):
    driver = selenium[browser_id]
    Wait(driver, WAIT_FRONTEND).until(
        EC.title_contains(text),
        message='seeing that page title contains {:s}'.format(text)
    )


@when(parsers.parse('user of {browser_id} types given name on keyboard'))
@then(parsers.parse('user of {browser_id} types given name on keyboard'))
def type_valid_name_string_into_active_element(selenium, browser_id,
                                               name_string):
    driver = selenium[browser_id]
    Wait(driver, WAIT_FRONTEND).until(
        lambda s: enter_text(s.switch_to.active_element, name_string),
        message='entering {:s} to input box'.format(name_string)
    )


@when(parsers.parse('user of {browser_id} types "{text}" on keyboard'))
@then(parsers.parse('user of {browser_id} types "{text}" on keyboard'))
def type_string_into_active_element(selenium, browser_id, text):
    driver = selenium[browser_id]
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
    driver = selenium[browser_id]
    item = tmp_memory[browser_id]['mailbox'][item_type]
    Wait(driver, WAIT_FRONTEND).until(
        lambda s: enter_text(s.switch_to.active_element, item),
        message='entering {:s} to input box'.format(item)
    )


@when(parsers.parse('user of {browser_id} presses enter on keyboard'))
@then(parsers.parse('user of {browser_id} presses enter on keyboard'))
def press_enter_on_active_element(selenium, browser_id):
    driver = selenium[browser_id]
    driver.switch_to.active_element.send_keys(Keys.RETURN)


@when(parsers.parse('user of {browser_id} presses backspace on keyboard'))
@then(parsers.parse('user of {browser_id} presses backspace on keyboard'))
def press_enter_on_active_element(selenium, browser_id):
    driver = selenium[browser_id]
    driver.switch_to.active_element.send_keys(Keys.BACKSPACE)


@then(parsers.parse('user of {browser_id} should see {links_names} links'))
def link_with_text_present(selenium, browser_id, links_names):
    driver = selenium[browser_id]
    for name in parse_seq(links_names):
        assert driver.find_element_by_link_text(name), \
            '{} link not found'.format(name)


@given(parsers.re('users? of (?P<browser_id_list>.*) clicked on the '
                  '"(?P<link_name>.*)" link'))
def g_click_on_link_with_text(selenium, browser_id_list, link_name):
    for browser_id in parse_seq(browser_id_list):
        driver = selenium[browser_id]
        driver.find_element_by_link_text(link_name).click()


@when(parsers.re('users? of (?P<browser_id_list>.*) clicks on the '
                 '"(?P<link_name>.*)" link'))
@then(parsers.re('users? of (?P<browser_id_list>.*) clicks on the '
                 '"(?P<link_name>.*)" link'))
def wt_click_on_link_with_text(selenium, browser_id_list, link_name):
    for browser_id in parse_seq(browser_id_list):
        driver = selenium[browser_id]
        driver.find_element_by_link_text(link_name).click()


@when(parsers.re('user of (?P<browser_id>.+?) is idle for '
                 '(?P<seconds>\d*\.?\d+([eE][-+]?\d+)?) seconds'))
@then(parsers.re('user of (?P<browser_id>.+?) is idle for '
                 '(?P<seconds>\d*\.?\d+([eE][-+]?\d+)?) seconds'))
def wait_n_seconds(seconds):
    time.sleep(float(seconds))


@then(parsers.parse('user of {browser_id} should see a page with "{text}" header'))
def page_with_header(selenium, browser_id, text):

    def header_with_text_presence(s):
        headers = s.find_elements_by_css_selector('h1, h2, h3, h4, h5')
        try:
            return any(map(lambda h: h.text == text, headers))
        except StaleElementReferenceException:
            return False

    driver = selenium[browser_id]
    Wait(driver, WAIT_BACKEND).until(header_with_text_presence)


@when(parsers.parse('user of {browser_id} sees an {notify} notify '
                    'with text matching to: {text_regexp}'))
@then(parsers.parse('user of {browser_id} sees an {notify} notify '
                    'with text matching to: {text_regexp}'))
def notify_visible_with_text(selenium, browser_id, notify, text_regexp):
    driver = selenium[browser_id]
    css_sel = '.ember-notify-show[class*={}] .message'.format(notify)

    @repeat_failed(timeout=2 * WAIT_BACKEND)
    def notify_with_text_present(d, sel, regexp):
        notifiers = d.find_elements_by_css_selector(sel)
        if notifiers:
            with suppress(NoSuchElementException, StaleElementReferenceException):
                matching_elements = [e for e in notifiers if regexp.match(e.text)]
                if len(matching_elements) > 0:
                    return
        else:
            raise AssertionError('no {} notify with "{}" msg '
                                 'found'.format(notify, text_regexp))

    notify_with_text_present(driver, css_sel, re.compile(text_regexp))


@when(parsers.parse('user of {browser_id} closes all notifies'))
@then(parsers.parse('user of {browser_id} closes all notifies'))
@repeat_failed(timeout=WAIT_FRONTEND)
def close_visible_notifies(selenium, browser_id):
    driver = selenium[browser_id]
    notifies = driver.find_elements_by_css_selector('.ember-notify '
                                                    'a.close-button')
    for notify_cls in notifies:
        with suppress(StaleElementReferenceException):
            notify_cls.click()
            Wait(driver, WAIT_BACKEND).until(
                staleness_of(notify_cls),
                message='waiting for notify to disappear'
            )


@when(parsers.parse('user of {browser_id} refreshes site'))
@then(parsers.parse('user of {browser_id} refreshes site'))
def refresh_site(selenium, browser_id):
    driver = selenium[browser_id]
    driver.refresh()


@when(parsers.parse('user of {browser_id} refreshes webapp'))
@then(parsers.parse('user of {browser_id} refreshes webapp'))
def refresh_site(selenium, browser_id):
    driver = selenium[browser_id]
    driver.get(parse_url(driver.current_url).group('base_url'))


# Below functions are currently unused and should not be used,
# because it involves a knowledge about internals...


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


@given(parsers.parse('there are no working provider(s) named {provider_list}'))
def kill_providers(persistent_environment, provider_list):
    kill_cmd = ['docker', 'kill']
    inspect_cmd = ['docker', 'inspect', '-f', '{{.State.Running}}']
    for provider in parse_seq(provider_list):
        for node in persistent_environment["op_worker_nodes"]:
            if provider in node:
                container_name = node.split('@')[1]
                subprocess.call(kill_cmd + [container_name])
                for _ in xrange(10):
                    is_alive = subprocess.Popen(inspect_cmd + [container_name],
                                                stdout=subprocess.PIPE)
                    with suppress(Exception):
                        if is_alive.communicate()[0] == 'false\n':
                            break
                    time.sleep(1)
                else:
                    raise RuntimeError('container {} still alive, while it '
                                       'should not be'.format(container_name))
