"""Common steps for Oneprovider.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


import pyperclip

from tests.gui.utils.generic import parse_seq
from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND, MAX_REFRESH_COUNT
from tests.gui.utils.generic import refresh_and_call, parse_url

from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.common.exceptions import NoSuchElementException

from pytest_bdd import given, parsers, when, then
from pytest_selenium_multi.pytest_selenium_multi import select_browser


MAIN_MENU_TAB_TO_URL = {'spaces': 'spaces',
                        'groups': 'groups',
                        'data': 'data',
                        'shared': 'shares'}


def _click_on_tab_in_main_menu_sidebar(driver, tab):
    def _load_main_menu_tab_page():
        def _check_url():
            try:
                found = parse_url(driver.current_url).group('tab')
            except AttributeError:
                return False
            else:
                return MAIN_MENU_TAB_TO_URL[tab] == found.lower()

        driver.find_element_by_css_selector(css_path).click()

        return Wait(driver, WAIT_FRONTEND).until(
            lambda _: _check_url(),
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
        driver = select_browser(selenium, browser_id)
        _click_on_tab_in_main_menu_sidebar(driver, main_menu_tab)


@when(parsers.re('users? of (?P<browser_id_list>.*) clicks on the '
                 '"(?P<main_menu_tab>.*)" tab in main menu sidebar'))
@then(parsers.re('users? of (?P<browser_id_list>.*) clicks on the '
                 '"(?P<main_menu_tab>.*)" tab in main menu sidebar'))
def wt_click_on_the_given_main_menu_tab(selenium, browser_id_list,
                                        main_menu_tab):
    for browser_id in parse_seq(browser_id_list):
        driver = select_browser(selenium, browser_id)
        _click_on_tab_in_main_menu_sidebar(driver, main_menu_tab)


@when(parsers.parse('user of {browser_id} refreshes Oneprovider site'))
@then(parsers.parse('user of {browser_id} refreshes Oneprovider site'))
def op_refresh_op_site_by_rm_hashtag(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    op_url = parse_url(driver.current_url).group('base_url')
    driver.get(op_url)


def _check_for_presence_of_item_in_table(driver, name, caption):
    table_elems = driver.find_elements_by_css_selector('table thead, '
                                                       'table tbody')
    for thead, tbody in zip(table_elems[::2], table_elems[1::2]):
        th = thead.find_element_by_css_selector('th .item-label')
        if th.text.lower() == caption.lower():
            items = tbody.find_elements_by_css_selector('.permissions-'
                                                        'table-row '
                                                        '.truncate')
            return any(item.text == name for item in items)


@when(parsers.parse('user of {browser_id} sees that "{name}" item has appeared '
                    'on current {caption} permissions table'))
@then(parsers.parse('user of {browser_id} sees that "{name}" item has appeared '
                    'on current {caption} permissions table'))
def op_check_if_row_of_name_appeared_in_table(selenium, browser_id,
                                              name, caption):
    driver = select_browser(selenium, browser_id)
    Wait(driver, MAX_REFRESH_COUNT * WAIT_BACKEND).until(
        lambda s: refresh_and_call(s, _check_for_presence_of_item_in_table,
                                   name, caption),
        message='searching for exactly one {:s} '
                'on {:s} list in table'.format(name, caption)
    )


def _wait_for_op_session_to_start(selenium, browser_id_list):
    def _check_url():
        try:
            found = parse_url(driver.current_url).group('access')
        except AttributeError:
            return False
        else:
            return 'onedata' == found.lower()

    for browser_id in parse_seq(browser_id_list):
        driver = select_browser(selenium, browser_id)
        Wait(driver, WAIT_BACKEND).until(
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


@when(parsers.parse('user of {browser_id} copies a first '
                    'resource {item} from URL'))
@then(parsers.parse('user of {browser_id} copies a first '
                    'resource {item} from URL'))
def cp_part_of_url(selenium, browser_id, item):
    driver = select_browser(selenium, browser_id)
    pyperclip.copy(parse_url(driver.current_url).group(item.lower()))


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
    driver = select_browser(selenium, browser_id)
    _wait_for_tab_main_content_to_load(driver)


@when(parsers.parse('user of {browser_id} sees that '
                    'tab main content has been loaded'))
@then(parsers.parse('user of {browser_id} sees that '
                    'tab main content has been loaded'))
def wt_has_tab_main_content_been_loaded(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    _wait_for_tab_main_content_to_load(driver)


def _has_dir_content_been_loaded(driver):
    # find_element_* throws exception if nothing found
    loader = driver.find_elements_by_css_selector('#main-content '
                                                  '.loader-area-'
                                                  'content-with-'
                                                  'secondary-top')
    if loader:
        loader = loader[0]
        Wait(driver, WAIT_BACKEND).until_not(
            lambda _: loader.is_displayed(),
            message='waiting for dir content to end loading'
        )


@given(parsers.parse('user of {browser_id} sees that content of current '
                     'directory has been loaded'))
def g_has_dir_content_been_loaded(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    _has_dir_content_been_loaded(driver)


@when(parsers.parse('user of {browser_id} sees that content of current '
                    'directory has been loaded'))
@then(parsers.parse('user of {browser_id} sees that content of current '
                    'directory has been loaded'))
def wt_has_dir_content_been_loaded(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    _has_dir_content_been_loaded(driver)


def _has_file_browser_been_loaded(driver):
    # find_element_* throws exception if nothing found
    loader = driver.find_elements_by_css_selector('#main-content '
                                                  '#content-scroll '
                                                  '.route-loading')
    if loader:
        loader = loader[0]
        Wait(driver, WAIT_BACKEND).until_not(
            lambda _: loader.is_displayed(),
            message='waiting for file browser to end loading'
        )


@given(parsers.parse('user of {browser_id} seen that file browser '
                     'has been loaded'))
def g_has_file_browser_been_loaded(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    _has_file_browser_been_loaded(driver)


@when(parsers.parse('user of {browser_id} sees that file browser '
                    'has been loaded'))
@then(parsers.parse('user of {browser_id} sees that file browser '
                    'has been loaded'))
def wt_has_file_browser_been_loaded(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    _has_file_browser_been_loaded(driver)
