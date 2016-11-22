"""Common steps for Oneprovider.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


import re
import pyperclip

from tests.utils.acceptance_utils import list_parser
from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND, MAX_REFRESH_COUNT
from tests.gui.utils.generic import refresh_and_call, click_on_element, parse_url

from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC

from pytest_bdd import given, parsers, when, then
from pytest_selenium_multi.pytest_selenium_multi import select_browser


main_menu_tab_to_url = {'spaces': 'spaces',
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
                return main_menu_tab_to_url[tab] == found.lower()

        menu_tab = main_menu_tab_to_url[tab]
        css_path = '.primary-sidebar a#main-{:s}'.format(menu_tab)
        driver.find_element_by_css_selector(css_path).click()

        return Wait(driver, WAIT_FRONTEND).until(
            lambda _: _check_url(),
            message='waiting for url to change.'
                    'Current url: {:s}'.format(driver.current_url)
        )

    Wait(driver, WAIT_BACKEND).until(
        lambda _: _load_main_menu_tab_page(),
        message='waiting for {:s} main menu tab page to load'
                ''.format(tab)
    )


@given(parsers.re('users? of (?P<browser_id_list>.*) clicked on the '
                  '"(?P<main_menu_tab>.*)" tab in main menu sidebar'))
def g_click_on_the_given_main_menu_tab(selenium, browser_id_list,
                                       main_menu_tab):
    for browser_id in list_parser(browser_id_list):
        driver = select_browser(selenium, browser_id)
        _click_on_tab_in_main_menu_sidebar(driver, main_menu_tab)


@when(parsers.re('users? of (?P<browser_id_list>.*) clicks on the '
                 '"(?P<main_menu_tab>.*)" tab in main menu sidebar'))
@then(parsers.re('users? of (?P<browser_id_list>.*) clicks on the '
                 '"(?P<main_menu_tab>.*)" tab in main menu sidebar'))
def wt_click_on_the_given_main_menu_tab(selenium, browser_id_list,
                                        main_menu_tab):
    for browser_id in list_parser(browser_id_list):
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


@given(parsers.parse('user of {browser_id} sees that main content '
                     'has ended loading'))
def op_check_if_main_content_has_been_reloaded(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR,
                                            '.common-loader-spinner')),
        message='wait for main content to end loading'
    )


def _wait_for_op_session_to_start(selenium, browser_id_list):
    def _check_url():
        try:
            found = parse_url(driver.current_url).group('access')
        except AttributeError:
            return False
        else:
            return 'onedata' == found.lower()

    for browser_id in list_parser(browser_id_list):
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


@when(parsers.parse('user of {browser_id} copies {item} visible in url'))
@then(parsers.parse('user of {browser_id} copies {item} visible in url'))
def cp_part_of_url(selenium, browser_id, item):
    driver = select_browser(selenium, browser_id)
    pyperclip.copy(parse_url(driver.current_url).group(item))
