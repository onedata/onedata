"""Common steps for Oneprovider.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from pytest_bdd import given, parsers, when, then
from common import select_button_from_buttons_by_name, refresh_and_call
from selector import is_active


def _click_given_tab_in_main_menu_sidebar(selenium, main_menu_tab):
    css_selector = '.primary-sidebar a#main-' + main_menu_tab
    find_tab = select_button_from_buttons_by_name(main_menu_tab, css_selector)
    Wait(selenium, WAIT_FRONTEND).until(find_tab).click()


@given(parsers.parse('user clicks on the "{main_menu_tab}" '
                     'tab in main menu sidebar'))
def g_op_click_on_the_given_main_menu_tab(selenium, main_menu_tab):
    _click_given_tab_in_main_menu_sidebar(selenium, main_menu_tab)


@then(parsers.parse('user clicks on the "{main_menu_tab}" '
                    'tab in main menu sidebar'))
def t_op_click_on_the_given_main_menu_tab(selenium, main_menu_tab):
    _click_given_tab_in_main_menu_sidebar(selenium, main_menu_tab)


@then('user sees non-empty token in active modal')
def op_check_for_non_empty_token_in_active_modal_on_op_page(selenium):
    assert Wait(selenium, WAIT_BACKEND).until(
        lambda s: s.find_element_by_css_selector(
            '.input-with-button '
            'div[id*=form-token] '
            'input')
    ).get_attribute('value')


@when(parsers.parse('user clicks on the "{option_name}" '
                    'button in {main_menu_tab} sidebar'))
@then(parsers.parse('user clicks on the "{option_name}" '
                    'button in {main_menu_tab} sidebar'))
def op_click_on_button_in_main_menu_tab_sidebar(selenium, option_name,
                                                main_menu_tab):
    assert main_menu_tab in ('spaces', 'groups')
    Wait(selenium, WAIT_FRONTEND).until(
        select_button_from_buttons_by_name(option_name, '.secondary-sidebar '
                                                        'figure.icon')
    ).click()


def _check_for_item_in_given_list(selenium, name, elem_type):
    list_items = selenium.find_elements_by_css_selector('.' + elem_type + '-list '
                                                        '.secondary-sidebar-item '
                                                        '.item-label .truncate')
    return sum(1 for li in list_items if li.text == name) == 1


@given(parsers.parse('there is a "{name}" item on the {elem_type} list'))
def op_check_if_there_is_given_item_on_the_list_of_given_type(selenium, name, elem_type):
    Wait(selenium, 3*WAIT_BACKEND).until(
        lambda s: refresh_and_call(s, _check_for_item_in_given_list,
                                   name, elem_type)
    )


@then(parsers.parse('user sees that the new item has appeared '
                    'on the {elem_type} list'))
def op_check_if_new_item_appears_in_list_of_given_type(selenium, elem_type,
                                                       name_string):
    Wait(selenium, 3*WAIT_BACKEND).until(
        lambda s: refresh_and_call(s, _check_for_item_in_given_list,
                                   name_string, elem_type)
    )


@then(parsers.parse('user sees that the "{name}" '
                    'has appeared on the {elem_type} list'))
def op_check_if_item_of_given_name_appears_in_list_of_given_type(selenium,
                                                                 elem_type,
                                                                 name):
    Wait(selenium, 3*WAIT_BACKEND).until(
        lambda s: refresh_and_call(s, _check_for_item_in_given_list,
                                   name, elem_type)
    )


@then(parsers.parse('user sees that the "{name}" '
                    'has vanished from the {elem_type} list'))
def op_check_if_item_of_given_name_appears_in_list_of_given_type(selenium,
                                                                 elem_type,
                                                                 name):
    def _check_for_lack_of_item_in_given_list(s):
        items_list = s.find_elements_by_css_selector('.' + elem_type + '-list '
                                                     '.secondary-sidebar-item '
                                                     '.item-label .truncate')
        return all(li.text != name for li in items_list)

    Wait(selenium, 3*WAIT_BACKEND).until(
        lambda s: refresh_and_call(s, _check_for_lack_of_item_in_given_list)
    )


def _find_item_in_given_sidebar_list(selenium, name, elem_type):
    item_list = selenium.find_elements_by_css_selector('.' + elem_type + '-list '
                                                       '.secondary-sidebar-item')
    for item in item_list:
        # if settings dropdown menu is expanded text looks like: name\noption1\noption2\n...
        # so splitting text on nl and getting 0 element
        item_name = item.text.split('\n')[0]  # TODO better way to check if it is the item we seek
        if item_name == name:
            return item


@when(parsers.parse('user clicks a settings icon displayed for '
                    '"{name}" item on the {elem_type} list'))
@then(parsers.parse('user clicks a settings icon displayed for '
                    '"{name}" item on the {elem_type} list'))
def op_click_settings_icon_for_given_list_item(selenium, name, elem_type):

    def _find_settings_icon_and_check_if_clickable(s):
        list_item = _find_item_in_given_sidebar_list(s, name, elem_type)
        settings_icon = list_item.find_element_by_css_selector('.oneicon-settings')
        if settings_icon.is_enabled():
            selenium.execute_script('arguments[0].scrollIntoView();', settings_icon)
            return settings_icon

    Wait(selenium, WAIT_FRONTEND).until(
        _find_settings_icon_and_check_if_clickable
    ).click()


@when(parsers.parse('user sees a settings dropdown menu for '
                    '"{name}" item on the {elem_type} list'))
@then(parsers.parse('user sees a settings dropdown menu for '
                    '"{name}" item on the {elem_type} list'))
def op_wait_for_settings_dropdown_menu(selenium, name, elem_type):

    def _find_expanded_menu(s):
        list_item = _find_item_in_given_sidebar_list(s, name, elem_type)
        toggle = list_item.find_element_by_css_selector('.dropdown-toggle')
        return toggle.get_attribute('aria-expanded') == 'true'

    Wait(selenium, WAIT_FRONTEND).until(_find_expanded_menu)


@when(parsers.parse('user clicks on the "{item_name}" item '
                    'in current settings dropdown'))
@then(parsers.parse('user clicks on the "{item_name}" item '
                    'in current settings dropdown'))
def op_click_on_given_item_in_current_settings_dropdown(selenium, item_name):
    Wait(selenium, WAIT_FRONTEND).until(
        select_button_from_buttons_by_name(item_name, '.settings-dropdown '
                                                      '.dropdown-menu-settings '
                                                      '.clickable')
    ).click()


@then(parsers.parse('user clicks "{button_name}" '
                    'confirmation button in displayed modal'))
@when(parsers.parse('user clicks "{button_name}" '
                    'confirmation button in displayed modal'))
def op_click_confirmation_button(selenium, button_name):
    Wait(selenium, WAIT_FRONTEND).until(
        select_button_from_buttons_by_name(button_name, '.modal-content '
                                                        'button.btn-primary')
    ).click()


@given('user sees that main content has ended loading')
def op_check_if_main_content_has_been_reloaded(selenium):
    Wait(selenium, WAIT_FRONTEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR,
                                            '.common-loader-spinner'))
    )


def _chech_if_modal_of_given_name_is_displayed(selenium, modal_name):
    modal_name = modal_name.lower()
    modals = selenium.find_elements_by_css_selector('.ember-view.modal')
    for modal in modals:
        name = modal.find_element_by_css_selector('.modal-title').text
        if modal_name == name.lower() and modal.is_displayed():
            return modal


@when(parsers.parse('user sees that input box in "{modal_name}" modal is active'))
@then(parsers.parse('user sees that input box in "{modal_name}" modal is active'))
def op_wait_for_active_input_box_in_modal_with_given_name(selenium, modal_name):
    modal = Wait(selenium, WAIT_FRONTEND).until(
        lambda s: _chech_if_modal_of_given_name_is_displayed(s, modal_name)
    )
    modal_input = modal.find_element_by_css_selector('input')
    Wait(selenium, WAIT_FRONTEND).until(
        lambda s: is_active(s, modal_input)
    )


@when(parsers.parse('user sees that token box in "{modal_name}" modal is active'))
@then(parsers.parse('user sees that token box in "{modal_name}" modal is active'))
def op_wait_for_token_box_in_modal_with_given_name(selenium, modal_name):
    modal = Wait(selenium, WAIT_FRONTEND).until(
        lambda s: _chech_if_modal_of_given_name_is_displayed(s, modal_name)
    )
    Wait(selenium, WAIT_BACKEND).until(
        EC.visibility_of(modal.find_element_by_css_selector('input'))
    )


@when(parsers.parse('user sees that "{modal_name}" modal has vanished'))
@then(parsers.parse('user sees that "{modal_name}" modal has vanished'))
def op_check_if_modal_with_input_box_disappeared(selenium, modal_name):
    Wait(selenium, WAIT_FRONTEND).until_not(
        lambda s: _chech_if_modal_of_given_name_is_displayed(selenium, modal_name)
    )
