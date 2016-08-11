"""Common steps for Oneprovider.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

# from tests.gui.steps import onezone_logged_in_common as onezone_session
# from tests.gui.steps import onezone_before_login as onezone_no_session
#
# from pytest_bdd import given, parsers

from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from selenium.webdriver.support.ui import WebDriverWait as Wait
from pytest_bdd import given, when, then, parsers

from common import select_button_from_buttons_by_name, check_if_element_is_active


@given(parsers.parse('user clicks on the "{op_elem}" Oneprovider\'s sidebar panel'))
def op_open_op_panel(selenium, op_elem):
    css_selector = '.primary-sidebar a#main-' + op_elem
    find_button = select_button_from_buttons_by_name(op_elem, css_selector)
    Wait(selenium, WAIT_FRONTEND).until(find_button).click()


@when(parsers.parse('user clicks on the settings button for "{list_elem}" in current sidebar'))
def op_click_on_settings_button_for_given_elem_in_elems_list_in_current_sidebar(selenium,
                                                                                list_elem,
                                                                                op_elem):
    def _locate_settings_button(s):
        elem_list = s.find_elements_by_css_selector('.' + op_elem + '-list '
                                                    '.secondary-sidebar-item')
        for elem in elem_list:
            if list_elem == elem.find_element_by_css_selector('.item-element'
                                                              '.item-label '
                                                              '.truncate').text:
                return elem.find_element_by_css_selector('.dropdown'
                                                         '.settings-dropdown')

    Wait(selenium, WAIT_BACKEND).until(_locate_settings_button).click()


@when(parsers.parse('user clicks on the "{option_name}" button in current sidebar'))
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


@then(parsers.parse('user should see, that the new {elem} appear on the list'))
def op_check_if_new_item_appeared_in_list_of_given_type_in_current_sidebar(selenium,
                                                                           elem,
                                                                           random_name):

    def header_with_text_presence(s):
        headers = s.find_elements_by_css_selector('.' + elem + 's-list '
                                                  '.secondary-sidebar-item '
                                                  '.item-label .truncate')
        return any(h.text == random_name for h in headers)

    Wait(selenium, WAIT_BACKEND).until(header_with_text_presence)


@when(parsers.parse('user clicks on the "{button}" button in current settings dropdown'))
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


@when(parsers.parse('user should see that "{box_title}" {modal_type} box on Oneprovider page is active'))
def op_wait_for_active_box_with_given_title_on_op_page(selenium, box_title, modal_type):
    if modal_type == 'input':
        wait = WAIT_FRONTEND
    elif modal_type == 'token':
        wait = WAIT_BACKEND
    else:
        raise AttributeError
    modals = selenium.find_elements_by_css_selector('.ember-view.modal')
    modal = _find_modal_by_title(box_title, modals)
    active_elem = modal.find_element_by_css_selector('input')
    is_active = check_if_element_is_active(web_elem=active_elem)
    Wait(selenium, wait).until(is_active)


# @given(parsers.parse('''I'm logged into Oneprovider "{provider}" as development user "{user}"'''))
# def logged_in_dev_to_oneprovider(selenium, base_url, user, provider):
#     onezone_no_session.login_dev_onezone_with_url(selenium, base_url, user)
#     onezone_session.uncollapse_main_accordion(selenium, 'providers')
#     onezone_session.go_to_provider(selenium, provider)
#     pass
