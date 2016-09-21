"""Common steps for Oneprovider.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
import pyperclip

from tests.utils.acceptance_utils import list_parser
from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND, MAX_REFRESH_COUNT, \
    WAIT_REFRESH
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from pytest_bdd import given, parsers, when, then
from selenium.webdriver.common.keys import Keys
from selenium.common.exceptions import TimeoutException

from ..utils.inspect import is_active
from ..utils.generic import refresh_and_call, click_on_element, parse_url
from pytest_selenium_multi.pytest_selenium_multi import select_browser


main_menu_tab_to_url = {'spaces': 'spaces',
                        'groups': 'groups',
                        'data': 'data',
                        'shared': 'shares'}


def _click_on_tab_in_main_menu_sidebar(driver, main_menu_tab):
    def _load_main_menu_tab_page():
        def _check_url():
            try:
                found = parse_url(driver.current_url).group('tab')
            except AttributeError:
                return False
            else:
                return main_menu_tab_to_url[main_menu_tab] == found.lower()

        click_on_element(driver, item_name=main_menu_tab,
                         css_path='.primary-sidebar a#main-'
                                  '{:s}'.format(main_menu_tab),
                         msg='clicking on {:s} tab in main menu')

        return Wait(driver, WAIT_FRONTEND).until(
            lambda _: _check_url(),
            message='waiting for url to change.'
                    'Current url: {:s}'.format(driver.current_url)
        )

    Wait(driver, WAIT_BACKEND).until(
        lambda _: _load_main_menu_tab_page(),
        message='waiting for {:s} main menu tab page to load'
                ''.format(main_menu_tab)
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


def _get_visible_token_from_active_modal(driver):
    return Wait(driver, WAIT_BACKEND).until(
        lambda s: s.find_element_by_css_selector(
            '.input-with-button '
            'input')
    ).get_attribute('value')


@when(parsers.parse('user of {browser_id} refreshes Oneprovider site'))
@then(parsers.parse('user of {browser_id} refreshes Oneprovider site'))
def op_refresh_op_site_by_rm_hashtag(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    op_url = parse_url(driver.current_url).group('base_url')
    driver.get(op_url)


@when(parsers.parse('user of {browser_id} selects "{item_name}" '
                    'from {item_type} list'))
@then(parsers.parse('user of {browser_id} selects "{item_name}" '
                    'from {item_type} list'))
def op_select_item_from_list(selenium, browser_id, item_name, item_type):
    driver = select_browser(selenium, browser_id)
    click_on_element(driver, item_name=item_name,
                     ignore_case=False,
                     css_path='ul.{:s}-list '
                              '.secondary-sidebar-item'.format(item_type),
                     msg='clicking on {{:s}} item in {type} '
                         'list'.format(type=item_type))


@when(parsers.parse('user of {browser_id} sees non-empty token in active modal'))
@then(parsers.parse('user of {browser_id} sees non-empty token in active modal'))
def op_check_for_non_empty_token_in_active_modal(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    assert _get_visible_token_from_active_modal(driver)


@when(parsers.parse('user of {browser_id} clicks on copy button next to '
                    'input box to copy visible token'))
@then(parsers.parse('user of {browser_id} clicks on copy button next to '
                    'input box to copy visible token'))
def op_copy_visible_token_to_clipboard(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, '.input-with-button '
                                                     'button.copy-btn'))
    ).click()


@when(parsers.parse('user of {browser_id} sends copied token '
                    'to users of {browser_list}'))
@then(parsers.parse('user of {browser_id} sends copied token '
                    'to users of {browser_list}'))
def op_send_visible_token_to_other_users(selenium, browser_id,
                                         browser_list, tmp_memory):
    select_browser(selenium, browser_id)
    token = pyperclip.paste()
    for browser in list_parser(browser_list):
        if browser in tmp_memory:
            tmp_memory[browser]['token'] = token
        else:
            tmp_memory[browser] = {'token': token}


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


def _check_for_item_in_given_list(driver, name, elem_type):
    def _assert_one_item_in_list(s, item_name, item_type):
        items = s.find_elements_by_css_selector('.{:s}-list .secondary-'
                                                'sidebar-item .item-label '
                                                '.truncate'.format(item_type))
        return sum(1 for li in items if li.text == item_name) == 1

    Wait(driver, MAX_REFRESH_COUNT * WAIT_BACKEND).until(
        lambda s: refresh_and_call(s, _assert_one_item_in_list,
                                   name, elem_type),
        message='searching for exactly one {item} '
                'on {list} list'.format(item=name, list=elem_type)
    )


@given(parsers.parse('that in {browser_id} there is an "{item_name}" '
                     'item on the {item_type} list'))
@given(parsers.parse('that in {browser_id} there is a "{item_name}" '
                     'item on the {item_type} list'))
def op_check_if_there_is_an_item_on_the_list(selenium, browser_id,
                                             item_name, item_type):
    driver = select_browser(selenium, browser_id)
    _check_for_item_in_given_list(driver, item_name, item_type)


@when(parsers.parse('user of {browser_id} sees that the new item has appeared '
                    'on the {item_type} list'))
@then(parsers.parse('user of {browser_id} sees that the new item has appeared '
                    'on the {item_type} list'))
def op_check_if_new_item_appeared_in_list(selenium, browser_id,
                                          item_type, name_string):
    driver = select_browser(selenium, browser_id)
    _check_for_item_in_given_list(driver, name_string, item_type)


@when(parsers.parse('user of {browser_id} sees that the "{item_name}" '
                    'has appeared on the {item_type} list'))
@then(parsers.parse('user of {browser_id} sees that the "{item_name}" '
                    'has appeared on the {item_type} list'))
def op_check_if_item_of_name_appeared_in_list(selenium, browser_id,
                                              item_name, item_type):
    driver = select_browser(selenium, browser_id)
    _check_for_item_in_given_list(driver, item_name, item_type)


# TODO uncomment when leave from group backend will be repaired
# @then(parsers.parse('user of {browser_id} sees that the "{item_name}" '
#                     'has disappeared from the {item_type} list'))
# def op_check_if_item_of_name_disappeared_from_list(selenium, browser_id,
#                                                    item_type, item_name):
#     def _check_for_lack_of_item_in_list(s):
#         items = s.find_elements_by_css_selector('.{:s}-list .secondary-'
#                                                 'sidebar-item .item-label '
#                                                 '.truncate'.format(item_type))
#         return all(item.text != item_name for item in items)
#
#     driver = select_browser(selenium, browser_id)
#     Wait(driver, MAX_REFRESH_COUNT*WAIT_BACKEND).until(
#         lambda s: refresh_and_call(s, _check_for_lack_of_item_in_list),
#         message='waiting for {item} to disappear from '
#                 '{list} list'.format(item=item_name, list=item_type)
#     )


# TODO rm when leave from group backend will be repaired
@then(parsers.parse('user of {browser_id} sees that the "{item_name}" '
                    'has disappeared from the {item_type} list'))
def op_check_if_item_of_name_disappeared_from_list(selenium, browser_id,
                                                   item_name, item_type):
    def _check_for_lack_of_item_in_list(s):
        items = s.find_elements_by_css_selector('.{:s}-list .secondary-'
                                                'sidebar-item .item-label '
                                                '.truncate'.format(item_type))
        return all(item.text != item_name for item in items)

    def _refresh_and_call():
        """Refresh browser and keep calling callback with given args
        until achieve expected result or timeout.
        """
        op_url = parse_url(driver.current_url).group('base_url')
        driver.get(op_url)
        _click_on_tab_in_main_menu_sidebar(driver, item_type)

        try:
            result = Wait(driver, WAIT_REFRESH).until(
                lambda s: _check_for_lack_of_item_in_list(s)
            )
        except TimeoutException:
            return None
        else:
            return result

    driver = select_browser(selenium, browser_id)
    Wait(driver, MAX_REFRESH_COUNT*WAIT_BACKEND).until(
        lambda s: _refresh_and_call(),
        message='waiting for {:s} to disappear from '
                'groups list'.format(item_name)
    )


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


def _find_item_in_sidebar_list(driver, item_name, item_type):
    items = driver.find_elements_by_css_selector('.' + item_type + '-list '
                                                 '.secondary-sidebar-item')
    for item in items:
        # if settings dropdown menu is expanded text looks like: name\noption1\noption2\n...
        # so splitting text on nl and getting 0 element
        if item_name == item.text.split('\n')[0]:  # TODO better way to check if it is the item we seek
            return item


@when(parsers.parse('user of {browser_id} clicks a settings icon displayed '
                    'for "{item_name}" item on the {item_type} list'))
@then(parsers.parse('user of {browser_id} clicks a settings icon displayed '
                    'for "{item_name}" item on the {item_type} list'))
def op_click_settings_icon_for_list_item(selenium, browser_id,
                                         item_name, item_type):

    def _find_settings_icon_and_check_if_clickable(s):
        list_item = _find_item_in_sidebar_list(s, item_name, item_type)
        icon = list_item.find_element_by_css_selector('.oneicon-settings')
        if icon.is_enabled():
            s.execute_script('arguments[0].scrollIntoView();', icon)
            return icon

    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until(
        _find_settings_icon_and_check_if_clickable,
        message='clicks on settings icon for {name} on {type} '
                'list'.format(name=item_name, type=item_type)
    ).click()


@when(parsers.parse('user of {browser_id} sees a settings dropdown menu for '
                    '"{name}" item on the {elem_type} list'))
@then(parsers.parse('user of {browser_id} sees a settings dropdown menu for '
                    '"{name}" item on the {elem_type} list'))
def op_wait_for_settings_dropdown_menu(selenium, browser_id, name, elem_type):

    def _find_expanded_menu(s):
        list_item = _find_item_in_sidebar_list(s, name, elem_type)
        toggle = list_item.find_element_by_css_selector('.dropdown-toggle')
        return toggle.get_attribute('aria-expanded') == 'true'

    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until(
        _find_expanded_menu,
        message='waiting for settings dropdown to expand'
    )


@when(parsers.parse('user of {browser_id} clicks on the "{item_name}" item '
                    'in current settings dropdown'))
@then(parsers.parse('user of {browser_id} clicks on the "{item_name}" item '
                    'in current settings dropdown'))
def op_click_on_item_in_current_settings_dropdown(selenium, browser_id,
                                                  item_name):
    driver = select_browser(selenium, browser_id)
    click_on_element(driver, item_name=item_name,
                     css_path='.settings-dropdown '
                              '.dropdown-menu-settings '
                              '.clickable',
                     msg='clicking on {:s} in current '
                         'settings dropdown')


@then(parsers.parse('user of {browser_id} clicks "{button_name}" '
                    'confirmation button in displayed modal'))
@when(parsers.parse('user of {browser_id} clicks "{button_name}" '
                    'confirmation button in displayed modal'))
def op_click_confirmation_button_in_displayed_modal(selenium, browser_id,
                                                    button_name):
    driver = select_browser(selenium, browser_id)
    click_on_element(driver, item_name=button_name,
                     css_path='.modal-content button',
                     msg='clicking on {:s} in displayed modal')


@given(parsers.parse('user of {browser_id} sees that main content '
                     'has ended loading'))
def op_check_if_main_content_has_been_reloaded(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR,
                                            '.common-loader-spinner')),
        message='wait for main content to end loading'
    )


def _chech_if_modal_of_name_is_displayed(driver, modal_name):
    modal_name = modal_name.lower()
    modals = driver.find_elements_by_css_selector('.ember-view.modal')
    for modal in modals:
        name = modal.find_element_by_css_selector('.modal-title').text
        if modal_name == name.lower() and modal.is_displayed():
            return modal


@when(parsers.parse('user of {browser_id} sees that input box in "{modal_name}" '
                    'modal is active'))
@then(parsers.parse('user of {browser_id} sees that input box in "{modal_name}" '
                    'modal is active'))
def op_wait_for_active_input_box_in_modal_with_given_name(selenium, browser_id,
                                                          modal_name):
    driver = select_browser(selenium, browser_id)
    modal = Wait(driver, WAIT_FRONTEND).until(
        lambda s: _chech_if_modal_of_name_is_displayed(s, modal_name),
        message='waiting for {:s} modal to appear'.format(modal_name)
    )
    modal_input = modal.find_element_by_css_selector('input')
    # activate input box
    modal_input.send_keys(Keys.NULL)

    Wait(driver, WAIT_FRONTEND).until(
        lambda s: is_active(s, modal_input),
        message='waiting for input box to become active'
    )


@when(parsers.parse('user of {browser_id} sees that token box in '
                    '"{modal_name}" modal is active'))
@then(parsers.parse('user of {browser_id} sees that token box in '
                    '"{modal_name}" modal is active'))
def op_wait_for_token_box_in_modal_with_name(selenium, browser_id,
                                             modal_name):
    driver = select_browser(selenium, browser_id)
    modal = Wait(driver, WAIT_FRONTEND).until(
        lambda s: _chech_if_modal_of_name_is_displayed(s, modal_name),
        message='waiting for {:s} modal to appear'.format(modal_name)
    )
    Wait(driver, WAIT_BACKEND).until(
        EC.visibility_of(modal.find_element_by_css_selector('input')),
        message='waiting for token to appear in input box'
    )


@when(parsers.parse('user of {browser_id} sees that "{modal_name}" '
                    'modal has disappeared'))
@then(parsers.parse('user of {browser_id} sees that "{modal_name}" '
                    'modal has disappeared'))
def op_check_if_modal_with_input_box_disappeared(selenium, browser_id,
                                                 modal_name):
    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until_not(
        lambda s: _chech_if_modal_of_name_is_displayed(s,
                                                       modal_name),
        message='waiting for {:s} modal to disappear'.format(modal_name)
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
