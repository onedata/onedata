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

from ..utils.inspect import is_active
from ..utils.generic import refresh_and_call, click_on_given_clickable_element


def _click_given_tab_in_main_menu_sidebar(selenium, main_menu_tab):
    click_on_given_clickable_element(selenium, item_name=main_menu_tab,
                                     css_path='.primary-sidebar a#main-'
                                              '{:s}'.format(main_menu_tab),
                                     msg='clicking on {:s} tab in main menu')


@given(parsers.parse('user clicks on the "{main_menu_tab}" '
                     'tab in main menu sidebar'))
def g_op_click_on_the_given_main_menu_tab(selenium, main_menu_tab):
    _click_given_tab_in_main_menu_sidebar(selenium, main_menu_tab)


@when(parsers.parse('user clicks on the "{main_menu_tab}" '
                    'tab in main menu sidebar'))
@then(parsers.parse('user clicks on the "{main_menu_tab}" '
                    'tab in main menu sidebar'))
def wt_op_click_on_the_given_main_menu_tab(selenium, main_menu_tab):
    _click_given_tab_in_main_menu_sidebar(selenium, main_menu_tab)


@then('user sees non-empty token in active modal')
def op_check_for_non_empty_token_in_active_modal_on_op_page(selenium):
    assert Wait(selenium, WAIT_BACKEND).until(
        lambda s: s.find_element_by_css_selector(
            '.input-with-button '
            'div[id*=form-token] '
            'input')
    ).get_attribute('value')


@when(parsers.parse('user clicks on the "{button_name}" '
                    'button in {main_menu_tab} sidebar'))
@then(parsers.parse('user clicks on the "{button_name}" '
                    'button in {main_menu_tab} sidebar'))
def op_click_on_button_in_main_menu_tab_sidebar(selenium, button_name,
                                                main_menu_tab):
    assert main_menu_tab in ('spaces', 'groups')

    click_on_given_clickable_element(selenium, item_name=button_name,
                                     css_path='.secondary-sidebar '
                                              'figure.icon',
                                     msg='clicking on {{:s}} '
                                         'in {tab}'.format(tab=main_menu_tab))


def _check_for_item_in_given_list(selenium, name, elem_type):
    def _find_item(s, item_name, item_type):
        items = s.find_elements_by_css_selector('.{:s}-list .secondary-'
                                                'sidebar-item .item-label '
                                                '.truncate'.format(item_type))
        return sum(1 for li in items if li.text == item_name) == 1

    Wait(selenium, 3*WAIT_BACKEND).until(
        lambda s: refresh_and_call(s, _find_item,
                                   name, elem_type),
        message='searching for exactly one {item} '
                'on {list} list'.format(item=name, list=elem_type)
    )


@given(parsers.parse('there is a "{item_name}" item on the {item_type} list'))
def op_check_if_there_is_given_item_on_the_list_of_given_type(selenium,
                                                              item_name,
                                                              item_type):
    _check_for_item_in_given_list(selenium, item_name, item_type)


@when(parsers.parse('user sees that the new item has appeared '
                    'on the {item_type} list'))
@then(parsers.parse('user sees that the new item has appeared '
                    'on the {item_type} list'))
def op_check_if_new_item_appears_in_list_of_given_type(selenium, item_type,
                                                       name_string):
    _check_for_item_in_given_list(selenium, name_string, item_type)


@when(parsers.parse('user sees that the "{item_name}" has appeared '
                    'on the {item_type} list'))
@then(parsers.parse('user sees that the "{item_name}" has appeared '
                    'on the {item_type} list'))
def op_check_if_item_of_given_name_appears_in_list_of_given_type(selenium,
                                                                 item_name,
                                                                 item_type):
    _check_for_item_in_given_list(selenium, item_name, item_type)


@then(parsers.parse('user sees that the "{item_name}" '
                    'has vanished from the {item_type} list'))
def op_check_if_item_of_given_name_appears_in_list_of_given_type(selenium,
                                                                 item_type,
                                                                 item_name):
    def _check_for_lack_of_item_in_given_list(s):
        items = s.find_elements_by_css_selector('.{:s}-list .secondary-'
                                                'sidebar-item .item-label '
                                                '.truncate'.format(item_type))
        return all(item.text != item_name for item in items)

    Wait(selenium, 3*WAIT_BACKEND).until(
        lambda s: refresh_and_call(s, _check_for_lack_of_item_in_given_list),
        message='waiting for {item} to disappear from '
                '{list} list'.format(item=item_name, list=item_type)
    )


def _find_item_in_given_sidebar_list(selenium, item_name, item_type):
    items = selenium.find_elements_by_css_selector('.' + item_type + '-list '
                                                   '.secondary-sidebar-item')
    for item in items:
        # if settings dropdown menu is expanded text looks like: name\noption1\noption2\n...
        # so splitting text on nl and getting 0 element
        if item_name == item.text.split('\n')[0]:  # TODO better way to check if it is the item we seek
            return item


@when(parsers.parse('user clicks a settings icon displayed for '
                    '"{item_name}" item on the {item__type} list'))
@then(parsers.parse('user clicks a settings icon displayed for '
                    '"{item_name}" item on the {item_type} list'))
def op_click_settings_icon_for_given_list_item(selenium, item_name, item_type):

    def _find_settings_icon_and_check_if_clickable(s):
        list_item = _find_item_in_given_sidebar_list(s, item_name, item_type)
        icon = list_item.find_element_by_css_selector('.oneicon-settings')
        if icon.is_enabled():
            selenium.execute_script('arguments[0].scrollIntoView();', icon)
            return icon

    Wait(selenium, WAIT_FRONTEND).until(
        _find_settings_icon_and_check_if_clickable,
        message='clicks on settings icon for {name} on {type} '
                'list'.format(name=item_name, type=item_type)
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

    Wait(selenium, WAIT_FRONTEND).until(
        _find_expanded_menu,
        message='waiting for settings dropdown to expand'
    )


@when(parsers.parse('user clicks on the "{item_name}" item '
                    'in current settings dropdown'))
@then(parsers.parse('user clicks on the "{item_name}" item '
                    'in current settings dropdown'))
def op_click_on_given_item_in_current_settings_dropdown(selenium, item_name):
    click_on_given_clickable_element(selenium, item_name=item_name,
                                     css_path='.settings-dropdown '
                                              '.dropdown-menu-settings '
                                              '.clickable',
                                     msg='clicking on {:s} in current '
                                         'settings dropdown')


@then(parsers.parse('user clicks "{button_name}" '
                    'confirmation button in displayed modal'))
@when(parsers.parse('user clicks "{button_name}" '
                    'confirmation button in displayed modal'))
def op_click_confirmation_button(selenium, button_name):
    click_on_given_clickable_element(selenium, item_name=button_name,
                                     css_path='.modal-content button',
                                     msg='clicking on {:s} in '
                                         'displayed modal')


@given('user sees that main content has ended loading')
def op_check_if_main_content_has_been_reloaded(selenium):
    Wait(selenium, WAIT_FRONTEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR,
                                            '.common-loader-spinner')),
        message='wait for main content to end loading'
    )


def _chech_if_modal_of_given_name_is_displayed(selenium, modal_name):
    modal_name = modal_name.lower()
    modals = selenium.find_elements_by_css_selector('.ember-view.modal')
    for modal in modals:
        name = modal.find_element_by_css_selector('.modal-title').text
        if modal_name == name.lower() and modal.is_displayed():
            return modal


@when(parsers.parse('user sees that input box in "{modal_name}" '
                    'modal is active'))
@then(parsers.parse('user sees that input box in "{modal_name}" '
                    'modal is active'))
def op_wait_for_active_input_box_in_modal_with_given_name(selenium, modal_name):
    modal = Wait(selenium, WAIT_FRONTEND).until(
        lambda s: _chech_if_modal_of_given_name_is_displayed(s, modal_name),
        message='waiting for {:s} modal to appear'.format(modal_name)
    )
    modal_input = modal.find_element_by_css_selector('input')
    Wait(selenium, WAIT_FRONTEND).until(
        lambda s: is_active(s, modal_input),
        message='waiting for input box to become active'
    )


@when(parsers.parse('user sees that token box in "{modal_name}" '
                    'modal is active'))
@then(parsers.parse('user sees that token box in "{modal_name}" '
                    'modal is active'))
def op_wait_for_token_box_in_modal_with_given_name(selenium, modal_name):
    modal = Wait(selenium, WAIT_FRONTEND).until(
        lambda s: _chech_if_modal_of_given_name_is_displayed(s, modal_name),
        message='waiting for {:s} modal to appear'.format(modal_name)
    )
    Wait(selenium, WAIT_BACKEND).until(
        EC.visibility_of(modal.find_element_by_css_selector('input')),
        message='waiting for token to appear in input box'
    )


@when(parsers.parse('user sees that "{modal_name}" modal has vanished'))
@then(parsers.parse('user sees that "{modal_name}" modal has vanished'))
def op_check_if_modal_with_input_box_disappeared(selenium, modal_name):
    Wait(selenium, WAIT_FRONTEND).until_not(
        lambda s: _chech_if_modal_of_given_name_is_displayed(selenium,
                                                             modal_name),
        message='waiting for {:s} modal to vanish'.format(modal_name)
    )
