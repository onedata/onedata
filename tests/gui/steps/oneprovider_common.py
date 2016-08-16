"""Common steps for Oneprovider.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.gui.steps.common import find_element_by_css_selector_and_text
from tests.gui.steps import onezone_logged_in_common as onezone_session
from tests.gui.steps import onezone_before_login as onezone_no_session
from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from pytest_bdd import given, parsers, when, then
from pytest import fixture
import selenium
from common import select_button_from_buttons_by_name, check_if_element_is_active


@given(parsers.parse('existing {space_name}'))
def existing_space_name(space_name):
    return space_name


def _click_tab_in_main_menu(selenium, op_elem):
    css_selector = '.primary-sidebar a#main-' + op_elem
    find_tab = select_button_from_buttons_by_name(op_elem, css_selector)
    Wait(selenium, WAIT_FRONTEND).until(find_tab).click()


@then(parsers.parse('user clicks on the "{op_elem}" tab in main menu'))
def t_op_open_op_panel(selenium, op_elem):
    _click_tab_in_main_menu(selenium, op_elem)


@given(parsers.parse('user clicks on the "{op_elem}" tab in main menu'))
def g_op_open_op_panel(selenium, op_elem):
    _click_tab_in_main_menu(selenium, op_elem)


@when(parsers.parse('user clicks on the "{option_name}" button in current sidebar'))
@then(parsers.parse('user clicks on the "{option_name}" button in current sidebar'))
def op_click_on_button_in_current_sidebar(selenium, option_name):
    selector = '.secondary-sidebar-header figure.icon'
    find_button = select_button_from_buttons_by_name(option_name, selector)
    Wait(selenium, WAIT_FRONTEND).until(find_button).click()


@then('user should see non-empty token in active window on Oneprovider page')
def op_can_see_non_empty_token_in_active_window_on_op_page(selenium):
    assert Wait(selenium, WAIT_BACKEND).until(
        lambda s: s.find_element_by_css_selector(
            '.input-with-button '
            'div[id*=form-token] '
            'input')
    ).get_attribute('value')


@when(parsers.parse('user clicks on the "{button}" button in current settings dropdown'))
@then(parsers.parse('user clicks on the "{button}" button in current settings dropdown'))
def op_click_on_button_in_current_settings_dropdown(selenium, button):
    selector = '.settings-dropdown .dropdown-menu-settings .clickable'
    find_button = select_button_from_buttons_by_name(button, selector)
    Wait(selenium, WAIT_FRONTEND).until(find_button).click()


def _find_modal_by_title(title, modals):
    for modal in modals:
        modal_name = modal.find_element_by_css_selector('.modal-title').text
        if modal_name == title:
            return modal
    return None


@then(parsers.parse('user should not see modal with title "{modal_title}"'))
def op_check_if_modal_with_input_box_disappeared(selenium, modal_title):
    modals = selenium.find_elements_by_css_selector('.ember-view.modal')
    Wait(selenium, WAIT_FRONTEND).until(
        lambda s: _find_modal_by_title(modal_title, modals) is None)
    Wait(selenium, WAIT_FRONTEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR, '.modal-backdrop-fade'))
    )


@when(parsers.parse('user should see that "{box_title}" {modal_type} box on Oneprovider page is active'))
@then(parsers.parse('user should see that "{box_title}" {modal_type} box on Oneprovider page is active'))
def op_wait_for_active_box_with_given_title_on_op_page(selenium, box_title, modal_type):
    if modal_type == 'input':
        wait = WAIT_FRONTEND
    elif modal_type == 'token':
        wait = WAIT_BACKEND
    else:
        raise AttributeError
    modals = selenium.find_elements_by_css_selector('.ember-view.modal')
    Wait(selenium, WAIT_FRONTEND).until(lambda s: _find_modal_by_title(box_title, modals) is not None)
    modal = _find_modal_by_title(box_title, modals)
    active_elem = modal.find_element_by_css_selector('input')
    is_active = check_if_element_is_active(web_elem=active_elem)
    Wait(selenium, wait).until(is_active)


@then(parsers.parse('user should see that the "{elem}" appears on the list'))
def op_check_if_new_item_appeared_in_list_of_given_type_in_current_sidebar(selenium,
                                                                           elem):

    def header_with_text_presence(s):
        headers = s.find_elements_by_css_selector('.spaces-list '
                                                  '.secondary-sidebar-item '
                                                  '.item-label .truncate')
        return any(h.text == elem for h in headers)

    Wait(selenium, WAIT_BACKEND).until(header_with_text_presence)


@then(parsers.parse('user should see that the new {elem} appears on the list'))
def op_check_if_new_item_appeared_in_list_of_given_type_in_current_sidebar(selenium,
                                                                           elem,
                                                                           random_name):

    def header_with_text_presence(s):
        headers = s.find_elements_by_css_selector('.' + elem + 's-list '
                                                  '.secondary-sidebar-item '
                                                  '.item-label .truncate')
        return any(h.text == random_name for h in headers)

    Wait(selenium, WAIT_BACKEND).until(header_with_text_presence)


@when(parsers.parse('user clicks settings icon displayed on "{name}" in current sidebar'))
@then(parsers.parse('user clicks settings icon displayed on "{name}" in current sidebar'))
def op_click_settings_icon_on_element(selenium, name):

    def _find_settings_icon_and_check_if_clickable(s):
        elems = s.find_elements_by_css_selector('.secondary-sidebar-item')
        for elem in elems:
            if elem.text == name:
                settings_icon = elem.find_element_by_css_selector('span.oneicon-settings')
                if settings_icon.is_enabled():
                    return settings_icon
        return None

    Wait(selenium, WAIT_FRONTEND).until(_find_settings_icon_and_check_if_clickable).click()


@when('user should see settings drop down menu for spaces')
@then('user should see settings drop down menu for spaces')
def op_wait_for_settings_dropdown_menu(selenium):

    def _find_expanded_menu(s):
        elems = s.find_elements_by_css_selector('.dropdown-toggle')
        for elem in elems:
            if elem.get_attribute('aria-expanded') == 'true':
                return elem
        return None

    Wait(selenium, WAIT_FRONTEND).until(lambda s: _find_expanded_menu is not None)


#@then(parsers.parse('user clicks settings icon displayed on name in current sidebar'))
#def click_settings_icon_on_element(selenium, name_string):
#    element = find_element_by_css_selector_and_text('.secondary-sidebar-item', name_string)
#    settings_icon = space.find_element_by_css_selector('span.oneicon-settings')
#    settings_icon.click()


@when(parsers.parse('user clicks "{button_name}" confirmation button in displayed modal'))
def op_click_confirmation_button(selenium, button_name):
    confirmation_button = select_button_from_buttons_by_name(button_name,
                                                             '.modal-content button.btn-primary')
    Wait(selenium, WAIT_FRONTEND).until(confirmation_button).click()


#@given('user should see that main content has been reloaded')
#def op_check_if_main_content_has_been_reloaded(selenium):
#    Wait(selenium, WAIT_FRONTEND).until(
#        EC.invisibility_of_element_located((By.CSS_SELECTOR, '.common-loader-spinner'))
#    )
