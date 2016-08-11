"""Steps for features of Oneprovider's groups.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from selenium.webdriver.support.ui import WebDriverWait as Wait
from pytest_bdd import given, when, then, parsers

from common import check_if_element_is_active
from common import select_button_from_buttons_by_name


@when(parsers.parse('user clicks on the "{name}" button'))
def click_on_button_in_secondary_sidebar_header(selenium, name):
    selector = '.secondary-sidebar-header figure.icon'
    find_button = select_button_from_buttons_by_name(name, selector)
    Wait(selenium, WAIT_FRONTEND).until(find_button).click()


@then(parsers.parse('user should see, that the new group appear on the list'))
def check_groups_names_headers_whether_new_group_appeared(selenium, random_name):

    def header_with_text_presence(s):
        headers = s.find_elements_by_css_selector('.groups-list .secondary-sidebar-item .item-label .truncate')
        return any(h.text == random_name for h in headers)

    Wait(selenium, WAIT_BACKEND).until(header_with_text_presence)


@when(parsers.parse('user clicks on the settings button for "{name}"'))
def click_on_settings_button_in_group_panel(selenium, name):
    def _locate_settings_button(s):
        groups = s.find_elements_by_css_selector('.groups-list '
                                                 '.secondary-sidebar-item')
        for group in groups:
            g_name = group.find_element_by_css_selector('.item-element'
                                                        '.item-label '
                                                        '.truncate').text
            if name == g_name:
                return group.find_element_by_css_selector('.dropdown'
                                                          '.settings-dropdown')

    Wait(selenium, WAIT_FRONTEND).until(_locate_settings_button).click()


@when(parsers.parse('user clicks on the "{button}" button in setting panel'))
def click_on_button_in_groups_settings_panel(selenium, button):
    selector = '.settings-dropdown .dropdown-menu-settings .clickable'
    find_button = select_button_from_buttons_by_name(button, selector)
    Wait(selenium, WAIT_FRONTEND).until(find_button).click()


@when('user should see that name input box is active')
def wait_for_input_box(selenium):
    is_active = check_if_element_is_active('#create-group-modal input')
    Wait(selenium, WAIT_FRONTEND).until(is_active)


@when('user should see that rename input box is active')
def wait_for_rename_input_box(selenium):
    is_active = check_if_element_is_active('#rename-group-modal input')
    Wait(selenium, WAIT_FRONTEND).until(is_active)


@when('user should see that join space input box is active')
def wait_for_join_space_input_box(selenium):
    is_active = check_if_element_is_active('#join-group-to-space-modal input')
    Wait(selenium, WAIT_FRONTEND).until(is_active)


@when('user should see that join as subgroup input box is active')
def wait_for_join_as_subgroup_input_box(selenium):
    is_active = check_if_element_is_active('#join-group-to-group-modal input')
    Wait(selenium, WAIT_FRONTEND).until(is_active)


@when('user should see that join group input box is active')
def wait_for_join_group_input_box(selenium):
    is_active = check_if_element_is_active('#join-group-modal input')
    Wait(selenium, WAIT_FRONTEND).until(is_active)


@when('user should see that request input box is active')
@when('user should see that invite input box is active')
def wait_for_request_or_invite_box(selenium):
    is_active = check_if_element_is_active('#token-undefined-modal input')
    Wait(selenium, WAIT_FRONTEND).until(is_active)


@then('user can copy visible invite-user token')
def copy_invite_user_token(selenium):
    assert Wait(selenium, WAIT_BACKEND).until(
        lambda s: s.find_element_by_css_selector(
            '.input-with-button '
            '#invite-form-token-userJoinGroup '
            'input#invite-form-token-userJoinGroup-field')
    ).get_attribute('value')


@then('user can copy visible invite-group token')
def copy_invite_group_token(selenium):
    assert Wait(selenium, WAIT_BACKEND).until(
        lambda s: s.find_element_by_css_selector(
            '.input-with-button '
            '#invite-form-token-groupJoinGroup '
            'input#invite-form-token-groupJoinGroup-field')
    ).get_attribute('value')


@then('user can copy visible request token')
def copy_request_token(selenium):
    assert Wait(selenium, WAIT_BACKEND).until(
        lambda s: s.find_element_by_css_selector(
            '.input-with-button '
            '#invite-form-token-requestSpaceCreation '
            'input#invite-form-token-requestSpaceCreation-field')
    ).get_attribute('value')


# sprawdzanie file dist czy jest ten prov na ktorego sie weszlo



# @when('user types new group name on keyboard')
# def clear_and_type_string_into_active_element(selenium, random_name):
#     selenium.switch_to.active_element.clear()
#     selenium.switch_to.active_element.send_keys(random_name)
#
#
# @then('user should see popup with information about name change')
# def check_confirmation_after_rename(selenium, name, random_name):
#     from common import notify_visible_with_text
#     notify_visible_with_text(selenium, "info", '.*' + name + '.*renamed.*' + random_name + '.*')
#
#
# @then(parsers.parse('user should see, that the new name replaced old one on the list'))
# def renamed_group(selenium, name, random_name):
#
#     def header_with_text_presence(s):
#         headers = s.find_elements_by_css_selector('.groups-list .secondary-sidebar-item .item-label .truncate')
#         return all(h.text != name for h in headers) and any(h.text == random_name for h in headers)
#
#     Wait(selenium, WAIT_BACKEND).until(header_with_text_presence)
#
#     # restore previous name
#     click_on_settings_button_in_group_panel(selenium, random_name)
#     click_on_elem(selenium, "RENAME")
#     wait_for_rename_input_box(selenium)
#     clear_and_type_string_into_active_element(selenium, name)
#     selenium.switch_to.active_element.send_keys(Keys.RETURN)
