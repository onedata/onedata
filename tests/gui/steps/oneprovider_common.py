"""Common steps for Oneprovider.
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import time

from tests.gui.utils.generic import parse_seq, find_web_elem, repeat_failed
from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from tests.gui.utils.generic import parse_url

from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.common.exceptions import NoSuchElementException

from pytest_bdd import given, parsers, when, then


MAIN_MENU_TAB_TO_URL = {'spaces': 'spaces',
                        'groups': 'groups',
                        'data': 'data',
                        'shared': 'shares',
                        'providers': 'providers'}


def _click_on_tab_in_main_menu_sidebar(driver, tab):
    def _load_main_menu_tab_page():
        def _check_url(url):
            return url != driver.current_url

        current_url = driver.current_url
        driver.find_element_by_css_selector(css_path).click()

        return Wait(driver, WAIT_FRONTEND).until(
            lambda _: _check_url(current_url),
            message='waiting for url to change.'
                    'Current url: {:s}'.format(driver.current_url)
        )

    menu_tab = MAIN_MENU_TAB_TO_URL[tab]
    css_path = '.primary-sidebar a#main-{:s}'.format(menu_tab)

    Wait(driver, WAIT_BACKEND).until(
        lambda _: _load_main_menu_tab_page(),
        message='waiting for {:s} main menu tab page to load'
                ''.format(tab)
    )


@given(parsers.re('users? of (?P<browser_id_list>.*) clicked on the '
                  '"(?P<main_menu_tab>.*)" tab in main menu sidebar'))
def g_click_on_the_given_main_menu_tab(selenium, browser_id_list,
                                       main_menu_tab):
    for browser_id in parse_seq(browser_id_list):
        driver = selenium[browser_id]
        _click_on_tab_in_main_menu_sidebar(driver, main_menu_tab)


@when(parsers.re('users? of (?P<browser_id_list>.*) clicks on the '
                 '"(?P<main_menu_tab>.*)" tab in main menu sidebar'))
@then(parsers.re('users? of (?P<browser_id_list>.*) clicks on the '
                 '"(?P<main_menu_tab>.*)" tab in main menu sidebar'))
def wt_click_on_the_given_main_menu_tab(selenium, browser_id_list,
                                        main_menu_tab):
    for browser_id in parse_seq(browser_id_list):
        driver = selenium[browser_id]
        _click_on_tab_in_main_menu_sidebar(driver, main_menu_tab)


@when(parsers.parse('user of {browser_id} sees that provider name displayed in '
                    'Oneprovider page has value of "{val}"'))
@then(parsers.parse('user of {browser_id} sees that provider name displayed in '
                    'Oneprovider page has value of "{val}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_assert_provider_name_in_op(selenium, browser_id, val):
    css_sel = '.provider-name-container .provider-name'
    name = find_web_elem(selenium[browser_id], css_sel,
                         'no provider name found').text
    assert name == val, \
        'displayed {} provider name instead of expected {}'.format(name, val)


def _wait_for_op_session_to_start(selenium, browser_id_list):
    def _check_url():
        try:
            found = parse_url(driver.current_url).group('access')
        except AttributeError:
            return False
        else:
            return 'onedata' == found.lower()

    for browser_id in parse_seq(browser_id_list):
        driver = selenium[browser_id]

        # because of current subscription it is necessary
        # to wait under certain circumstances for things to properly work
        time.sleep(12)
        driver.get(parse_url(driver.current_url).group('base_url'))

        # TODO rm *4 when provider session starts becomes faster
        Wait(driver, WAIT_BACKEND*4).until(
            lambda _: _check_url(),
            message='waiting for session to start'
        )


@given(parsers.re('users? of (?P<browser_id_list>.*?) seen that '
                  'Oneprovider session has started'))
def g_wait_for_op_session_to_start(selenium, browser_id_list):
    _wait_for_op_session_to_start(selenium, browser_id_list)


@when(parsers.re('users? of (?P<browser_id_list>.*?) sees that '
                 'Oneprovider session has started'))
@then(parsers.re('users? of (?P<browser_id_list>.*?) sees that '
                 'Oneprovider session has started'))
def wt_wait_for_op_session_to_start(selenium, browser_id_list):
    _wait_for_op_session_to_start(selenium, browser_id_list)


def _wait_for_tab_main_content_to_load(driver):
    # find_element_* throws exception if nothing found
    try:
        loader = driver.find_element_by_css_selector('#main-content '
                                                     '.loader-area-'
                                                     'main-content'
                                                     ':not([class~=hidden])')
    except NoSuchElementException:
        return
    else:
        Wait(driver, WAIT_BACKEND).until_not(
            lambda _: loader.is_displayed(),
            message='waiting for tab main content to end loading'
        )


@given(parsers.parse('user of {browser_id} seen that '
                     'tab main content has been loaded'))
def g_has_tab_main_content_been_loaded(selenium, browser_id):
    driver = selenium[browser_id]
    _wait_for_tab_main_content_to_load(driver)


@when(parsers.parse('user of {browser_id} sees that '
                    'tab main content has been loaded'))
@then(parsers.parse('user of {browser_id} sees that '
                    'tab main content has been loaded'))
def wt_has_tab_main_content_been_loaded(selenium, browser_id):
    driver = selenium[browser_id]
    _wait_for_tab_main_content_to_load(driver)
