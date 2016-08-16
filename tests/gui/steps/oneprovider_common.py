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
from common import select_button_from_buttons_by_name, check_if_element_is_active


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


@then('user should see non-empty token in active modal')
def op_can_see_non_empty_token_in_active_modal_on_op_page(selenium):
    assert Wait(selenium, WAIT_BACKEND).until(
        lambda s: s.find_element_by_css_selector(
            '.input-with-button '
            'div[id*=form-token] '
            'input')
    ).get_attribute('value')


@then(parsers.parse('user should see that the {elem} "{name_string}" appears on the list'))
@then(parsers.parse('user should see that the new {elem} appears on the list'))
def op_check_if_new_item_appeared_in_list_of_given_type_in_current_sidebar(selenium,
                                                                           elem,
                                                                           name_string):

    def header_with_text_presence(s):
        headers = s.find_elements_by_css_selector('.' + elem + 's-list '
                                                  '.secondary-sidebar-item '
                                                  '.item-label .truncate')
        return any(h.text == name_string for h in headers)

    Wait(selenium, WAIT_BACKEND).until(header_with_text_presence)


@when(parsers.parse('user clicks a settings icon displayed for "{name}" list item'))
@then(parsers.parse('user clicks a settings icon displayed for "{name}" list item'))
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


@when(parsers.parse('user should see a settings dropdown menu for "{name}" list item'))
@then(parsers.parse('user should see a settings dropdown menu for "{name}" list item'))
@when('user should see settings drop down menu for spaces')
@then('user should see settings drop down menu for spaces')
def op_wait_for_settings_dropdown_menu(selenium, name):

    def _find_expanded_menu(s):
        elems = s.find_elements_by_css_selector('.secondary-sidebar-item')
        for elem in elems:
            if elem.text == name:
                toggle = elem.find_element_by_css_selector('.dropdown-toggle')
                return elem if toggle.get_attribute('aria-expanded') == 'true' else None

    Wait(selenium, WAIT_FRONTEND).until(lambda s: _find_expanded_menu is not None)


@when(parsers.parse('user clicks on the "{button}" button in sidebar panel'))
@then(parsers.parse('user clicks on the "{button}" button in sidebar panel'))
def op_click_on_button_in_current_sidebar(selenium, button):
    Wait(selenium, WAIT_FRONTEND).until(
        select_button_from_buttons_by_name(button, '.secondary-sidebar-header '
                                                   'figure.icon')
    ).click()


@when(parsers.parse('user clicks on the "{item_name}" item in current settings dropdown'))
@then(parsers.parse('user clicks on the "{item_name}" item in current settings dropdown'))
def op_click_on_button_in_current_settings_dropdown(selenium, item_name):
    Wait(selenium, WAIT_FRONTEND).until(
        select_button_from_buttons_by_name(item_name, '.settings-dropdown '
                                                   '.dropdown-menu-settings '
                                                   '.clickable')
    ).click()


@given(parsers.parse('there is a "{item_name}" item on a sidebar list'))
def check_if_there_is_given_item_on_list(selenium, item_name):
    def _find_elem_in_list(s):
        elements = s.find_elements_by_css_selector('.secondary-sidebar-item')
        for elem in elements:
            if elem.text == item_name:
                return elem

    Wait(selenium, WAIT_FRONTEND).until(_find_elem_in_list)


@when(parsers.parse('user clicks "{button_name}" confirmation button in displayed modal'))
def op_click_confirmation_button(selenium, button_name):
    confirmation_button = select_button_from_buttons_by_name(button_name,
                                                             '.modal-content button.btn-primary')
    Wait(selenium, WAIT_FRONTEND).until(confirmation_button).click()


def _find_modal_by_title(title, modals):
    for modal in modals:
        modal_name = modal.find_element_by_css_selector('.modal-title').text
        if modal_name.lower() == title.lower():
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


@when(parsers.parse('user should see that "{modal_title}" {modal_type} box is active'))
@then(parsers.parse('user should see that "{modal_title}" {modal_type} box is active'))
def op_wait_for_active_box_with_given_title_on_op_page(selenium, modal_title, modal_type):
    if modal_type == 'input':
        wait = WAIT_FRONTEND
    elif modal_type == 'token':
        wait = WAIT_BACKEND
    else:
        raise AttributeError
    modals = selenium.find_elements_by_css_selector('.ember-view.modal')
    modal = Wait(selenium, WAIT_FRONTEND).until(lambda _: _find_modal_by_title(modal_title, modals))
    is_active = check_if_element_is_active(
        web_elem=modal.find_element_by_css_selector('input'))
    Wait(selenium, wait).until(is_active)


@given(parsers.parse('existing {space_name}'))
def existing_space_name(space_name):
    return space_name


#@then(parsers.parse('user clicks settings icon displayed on name in current sidebar'))
#def click_settings_icon_on_element(selenium, name_string):
#    element = find_element_by_css_selector_and_text('.secondary-sidebar-item', name_string)
#    settings_icon = space.find_element_by_css_selector('span.oneicon-settings')
#    settings_icon.click()


# TODO not sure we need this function
# @given('user should see that main content has been reloaded')
# def op_check_if_main_content_has_been_reloaded(selenium):
#     Wait(selenium, WAIT_FRONTEND).until(
#         EC.invisibility_of_element_located((By.CSS_SELECTOR, '.common-loader-spinner'))
#     )
