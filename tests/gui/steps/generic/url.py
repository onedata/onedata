"""Steps for features of url handling.
"""

__author__ = "Jakub Liput, Bartosz WAlkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


import re
import pyperclip

from pytest_bdd import given, when, then, parsers
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support.expected_conditions import staleness_of
from pytest_selenium_multi.pytest_selenium_multi import select_browser

from tests.gui.utils.generic import parse_seq, repeat_failed, parse_url
from tests.gui.conftest import WAIT_BACKEND


@given(parsers.re("users? of (?P<browser_id_list>.*) opened Onezone URL"))
def g_visit_onezone(base_url, selenium, browser_id_list):
    for browser_id in parse_seq(browser_id_list):
        driver = select_browser(selenium, browser_id)
        driver.get(base_url)


@when(parsers.re('user of (?P<browser_id>.+) should be '
                 'redirected to (?P<page>.+) page'))
@then(parsers.re('user of (?P<browser_id>.+) should be '
                 'redirected to (?P<page>.+) page'))
def being_redirected_to_page(page, selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    Wait(driver, 5).until(
        lambda d: re.match(r'https?://.*?(/#)?(/.*)',
                           d.current_url).group(2) == page,
        message='{}'
    )


@when(parsers.re(r'user of (?P<browser_id>.+) changes '
                 r'the relative URL to (?P<path>.+)'))
@then(parsers.re(r'user of (?P<browser_id>.+) changes '
                 r'the relative URL to (?P<path>.+)'))
def visit_relative(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    driver.get(parse_url(driver.current_url).group('base_url') + path)


@when(parsers.re(r'user of (?P<browser_id>.*?) changes '
                 r'application path to plain (?P<path>.+)'))
@then(parsers.re(r'user of (?P<browser_id>.*?) changes '
                 r'application path to plain (?P<path>.+)'))
def on_ember_path(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    driver.get(parse_url(driver.current_url).group('base_url') + '/#' + path)


@when(parsers.re('user of (?P<browser_id>.+?) sees that '
                 '(?:url|URL) matches: (?P<path>.+)'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that '
                 '(?:url|URL) matches: (?P<path>.+)'))
def is_url_matching(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    regexp = r'{}$'.format(path.replace('\\', '\\\\'))
    err_msg = r'{} url is not like expected {}'

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True, interval=0.1)
    def assert_url_match(d, regex, msg):
        url = d.current_url
        assert re.match(regex, url), msg.format(url, regex)

    assert_url_match(driver, regexp, err_msg)


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
