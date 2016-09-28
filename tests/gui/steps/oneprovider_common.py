"""Common steps for Oneprovider.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import pyperclip

from tests.utils.acceptance_utils import list_parser
from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND, MAX_REFRESH_COUNT
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from pytest_bdd import given, parsers, when, then
from tests.gui.utils.generic import refresh_and_call, click_on_element, parse_url
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

        click_on_element(driver, item_name=tab,
                         css_path='.primary-sidebar a#main-{:s}'
                                  ''.format(main_menu_tab_to_url[tab]),
                         msg='clicking on {:s} tab in main menu')

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
def g_op_click_on_the_given_main_menu_tab(selenium, browser_id_list,
                                          main_menu_tab):
    for browser_id in list_parser(browser_id_list):
        driver = select_browser(selenium, browser_id)
        _click_on_tab_in_main_menu_sidebar(driver, main_menu_tab)


@when(parsers.re('users? of (?P<browser_id_list>.*) clicks on the '
                 '"(?P<main_menu_tab>.*)" tab in main menu sidebar'))
@then(parsers.re('users? of (?P<browser_id_list>.*) clicks on the '
                 '"(?P<main_menu_tab>.*)" tab in main menu sidebar'))
def wt_op_click_on_the_given_main_menu_tab(selenium, browser_id_list,
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


@when(parsers.parse('user of {browser_id} clicks on copy button next to '
                    'input box to copy visible token'))
@then(parsers.parse('user of {browser_id} clicks on copy button next to '
                    'input box to copy visible token'))
@when(parsers.parse('user of {browser_id} clicks on copy button next to '
                    'input box to copy visible url'))
@then(parsers.parse('user of {browser_id} clicks on copy button next to '
                    'input box to copy visible url'))
def op_copy_visible_token_to_clipboard(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, '.input-with-button '
                                                     'button.copy-btn'))
    ).click()


@when(parsers.parse('user of {browser_id} sends copied {item_type} '
                    'to users of {browser_list}'))
@then(parsers.parse('user of {browser_id} sends copied {item_type} '
                    'to users of {browser_list}'))
def op_send_visible_token_to_other_users(selenium, browser_id, item_type,
                                         browser_list, tmp_memory):
    select_browser(selenium, browser_id)
    item = pyperclip.paste()
    for browser in list_parser(browser_list):
        if browser in tmp_memory:
            tmp_memory[browser][item_type] = item
        else:
            tmp_memory[browser] = {item_type: item}


@when(parsers.parse('user of {browser_id} clicks on the "{button_name}" '
                    'button in {main_menu_tab} sidebar'))
@then(parsers.parse('user of {browser_id} clicks on the "{button_name}" '
                    'button in {main_menu_tab} sidebar'))
def op_click_on_button_in_main_menu_tab_sidebar(selenium, browser_id,
                                                button_name,
                                                main_menu_tab):
    driver = select_browser(selenium, browser_id)
    assert main_menu_tab in ('spaces', 'groups')

    click_on_element(driver, item_name=button_name,
                     css_path='.secondary-sidebar '
                              'figure.icon',
                     msg='clicking on {{:s}} '
                         'in {tab}'.format(tab=main_menu_tab))


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


@given(parsers.re('users? of (?P<browser_id_list>.*?) seen that '
                  'Oneprovider session has started'))
def wait_for_op_session_to_start(selenium, browser_id_list):
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
