"""This module contains gherkin steps to run acceptance tests featuring
url handling.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


import re

from pytest_bdd import given, when, then, parsers
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support.expected_conditions import staleness_of

from tests.gui.utils.generic import parse_seq, repeat_failed, parse_url
from tests.gui.conftest import WAIT_BACKEND


@given(parsers.parse("user of {browser_id_list} opened {hosts_list} page"))
@given(parsers.parse("users of {browser_id_list} opened {hosts_list} page"))
def g_open_onedata_service_page(selenium, browser_id_list, hosts_list, hosts):
    for browser_id, host in zip(parse_seq(browser_id_list),
                                parse_seq(hosts_list)):
        driver = selenium[browser_id]
        host = host.split()
        alias, service = host[0], '_'.join(host[1:])
        driver.get('https://{}'.format(hosts[service][alias]))


@when(parsers.re('user of (?P<browser_id>.+) should be '
                 'redirected to (?P<page>.+) page'))
@then(parsers.re('user of (?P<browser_id>.+) should be '
                 'redirected to (?P<page>.+) page'))
@repeat_failed(timeout=WAIT_BACKEND)
def being_redirected_to_page(page, selenium, browser_id):
    driver = selenium[browser_id]
    curr_page = re.match(r'https?://.*?(/#)?(/.*)',
                         driver.current_url).group(2)
    assert curr_page == page, ('currently on {} page instead of expected '
                               '{}'.format(curr_page, page))


@when(parsers.re(r'user of (?P<browser_id>.+) changes '
                 r'the relative URL to (?P<path>.+)'))
@then(parsers.re(r'user of (?P<browser_id>.+) changes '
                 r'the relative URL to (?P<path>.+)'))
def change_relative_url(selenium, browser_id, path):
    driver = selenium[browser_id]
    driver.get(parse_url(driver.current_url).group('base_url') + path)


@when(parsers.re(r'user of (?P<browser_id>.*?) changes '
                 r'application path to plain (?P<path>.+)'))
@then(parsers.re(r'user of (?P<browser_id>.*?) changes '
                 r'application path to plain (?P<path>.+)'))
def change_application_path(selenium, browser_id, path):
    driver = selenium[browser_id]
    driver.get(parse_url(driver.current_url).group('base_url') + '/#' + path)


@when(parsers.re('user of (?P<browser_id>.+?) sees that '
                 '(?:url|URL) matches: (?P<path>.+)'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that '
                 '(?:url|URL) matches: (?P<path>.+)'))
def is_url_matching(selenium, browser_id, path):
    driver = selenium[browser_id]
    regexp = r'{}$'.format(path.replace('\\', '\\\\'))
    err_msg = r'expected url: {} does not match current one: {{}}'.format(path)

    @repeat_failed(timeout=WAIT_BACKEND)
    def assert_url_match(d, regex, msg):
        curr_url = d.current_url
        assert re.match(regex, curr_url), msg.format(curr_url)

    assert_url_match(driver, regexp, err_msg)


@when(parsers.re('user of (?P<browser_id>.+?) opens received (?:url|URL)'))
@then(parsers.re('user of (?P<browser_id>.+?) opens received (?:url|URL)'))
def open_received_url(selenium, browser_id, tmp_memory, base_url):
    driver = selenium[browser_id]

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
def change_app_path_with_copied_item(selenium, browser_id, path,
                                     displays, clipboard):
    driver = selenium[browser_id]
    base_url = parse_url(driver.current_url).group('base_url')
    item = clipboard.paste(display=displays[browser_id])
    driver.get('{base_url}{path}/{item}'.format(base_url=base_url,
                                                path=path, item=item))


@when(parsers.re(r'user of (?P<browser_id>.*?) changes webapp path to '
                 r'(?P<path>.+?) concatenated with received (?P<item>.*)'))
@then(parsers.re(r'user of (?P<browser_id>.*?) changes webapp path to '
                 r'(?P<path>.+?) concatenated with received (?P<item>.*)'))
def change_app_path_with_recv_item(selenium, browser_id, path,
                                   tmp_memory, item):
    driver = selenium[browser_id]
    base_url = parse_url(driver.current_url).group('base_url')
    item = tmp_memory[browser_id]['mailbox'][item.lower()]
    driver.get('{base_url}{path}/{item}'.format(base_url=base_url,
                                                path=path, item=item))


@when(parsers.parse('user of {browser_id} copies url '
                    'from browser\'s location bar'))
@then(parsers.parse('user of {browser_id} copies url '
                    'from browser\'s location bar'))
def copy_site_url(selenium, browser_id, displays, clipboard):
    driver = selenium[browser_id]
    clipboard.copy(driver.current_url, display=displays[browser_id])


@when(parsers.parse('user of {browser_id} opens copied URL '
                    'in browser\'s location bar'))
@then(parsers.parse('user of {browser_id} opens copied URL '
                    'in browser\'s location bar'))
def open_site_url(selenium, browser_id, displays, clipboard):
    driver = selenium[browser_id]
    driver.get(clipboard.paste(display=displays[browser_id]))
