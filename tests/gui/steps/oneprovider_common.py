"""Common steps for Oneprovider.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

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

# @given(parsers.parse('''I'm logged into Oneprovider "{provider}" as development user "{user}"'''))
# def logged_in_dev_to_oneprovider(selenium, base_url, user, provider):
#     onezone_no_session.login_dev_onezone_with_url(selenium, base_url, user)
#     onezone_session.uncollapse_main_accordion(selenium, 'providers')
#     onezone_session.go_to_provider(selenium, provider)
#     pass

@fixture
def curr_url(selenium):
    return [selenium.current_url]


@given(parsers.parse('existing {space_name}'))
def existing_space_name(space_name):
    return 'space1'


@when('user sees current url')
def set_current_url(selenium, curr_url):
    curr_url[0] = selenium.current_url


@when('user clicks "Spaces" button')
def click_spaces_button(selenium):
    Wait(selenium, WAIT_FRONTEND).until(
        EC.visibility_of_element_located((By.CSS_SELECTOR, 'a#main-spaces'))).click()


@when('user clicks "Create" button')
def click_create_button(selenium):
    Wait(selenium, WAIT_BACKEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, 'span.oneicon-space-add'))).click()


@when('user clicks "Join" button')
def click_join_button(selenium):
    Wait(selenium, WAIT_BACKEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR,
                                    '.secondary-sidebar-header figure.icon span.oneicon-join'))).click()


@then(parsers.parse('user moves cursor on the "{space}"'))
@when(parsers.parse('user moves cursor on the "{space}"'))
def move_cursor_on_the_space(selenium, space):

    def find_space(s):
        links = s.find_elements_by_css_selector('ul.spaces-list .secondary-sidebar-item .truncate')
        for elem in links:
             if elem.text == space:
                return elem
        return None

    ActionChains(selenium).move_to_element(find_space(selenium))


@then('user clicks "Settings" icon')
@when('user clicks "Settings" icon')
def click_settings_icon(selenium):
    selenium.find_element_by_css_selector('span.oneicon-settings').click()


@then('user sees settings dropdown panel')
@when('user sees settings dropdown panel')
def wait_for_settings_panel(selenium):
    Wait(selenium, WAIT_BACKEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, '.settings-dropdown ul.dropdown-menu-list span.item-label'))
    )


@then(parsers.parse('user clicks "{option}" option'))
@when(parsers.parse('user clicks "{option}" option'))
def click_rename_option(selenium, option):

    def get_option_button(selenium):
        links = selenium.find_elements_by_css_selector('ul.spaces-list .dropdown .dropdown-menu li.clickable')
        for elem in links:
            if elem.text == option:
                return elem
        return None

    rename_button = Wait(selenium, WAIT_FRONTEND).until(get_option_button)
    rename_button.click()


@then('user should see, that rename input box is active')
@when('user should see, that rename input box is active')
def wait_rename_input_box_is_active(selenium):
    def _is_active(selenium):
        elem = selenium.find_elements_by_css_selector('div#rename-space-modal input')
        if elem:
            elem = elem[0]
            return elem == selenium.switch_to.active_element
        else:
            return False
    Wait(selenium, WAIT_FRONTEND).until(_is_active)


@when('user should see, that name input box is active')
def wait_name_input_box_is_active(selenium):

    def _is_active(selenium):
        elem = selenium.find_elements_by_css_selector('div#create-space-modal input')
        if elem:
            elem = elem[0]
            return elem == selenium.switch_to.active_element
        else:
            return False

    Wait(selenium, WAIT_FRONTEND).until(_is_active)


@when('user should see, that invite user token box is active')
def wait_invite_user_token_input_box_is_active(selenium):

    def _is_active(selenium):
        elem = selenium.find_elements_by_css_selector('.input-with-button input#invite-form-token-userJoinSpace-field')
        if elem:
            elem = elem[0]
            return elem == selenium.switch_to.active_element
        else:
            return False

    Wait(selenium, WAIT_FRONTEND).until(_is_active)


@when('user should see, that token input box is active')
def wait_token_input_box_is_active(selenium):

    def _is_active(selenium):
        elem = selenium.find_elements_by_css_selector('.form-group input.form-control')
        if elem:
            elem = elem[0]
            return elem == selenium.switch_to.active_element
        else:
            return False

    Wait(selenium, WAIT_FRONTEND).until(_is_active)


@when('user should see, that space creation token box is active')
def wait_creation_token_input_box_is_active(selenium):

    def _is_active(selenium):
        elem = selenium.find_elements_by_css_selector('.input-with-button input#invite-form-token-providerSupport-field')
        if elem:
            elem = elem[0]
            return elem == selenium.switch_to.active_element
        else:
            return False

    Wait(selenium, WAIT_FRONTEND).until(_is_active)


@when('user should see, that invite group token box is active')
def wait_invite_group_token_is_active(selenium):

    def _is_active(selenium):
        elem = selenium.find_elements_by_css_selector(
            '.input-with-button input#invite-form-token-groupJoinSpace-field')
        if elem:
            elem = elem[0]
            return elem == selenium.switch_to.active_element
        else:
            return False

    Wait(selenium, WAIT_FRONTEND).until(_is_active)


@when(parsers.parse('user clicks "{space_name}" button'))
def space_name_click(selenium, space_name):

    def get_space_to_click(s):
        spaces = s.find_elements_by_css_selector('ul.spaces-list .secondary-sidebar-item')
        for elem in spaces:
            if elem.text == space_name:
                return elem
        return None

    Wait(selenium, WAIT_BACKEND).until(get_space_to_click).click()


@then(parsers.parse('user should see new space "{name}"'))
def space_name_equals(selenium, name):

    def find_created_space(s):
        links = s.find_elements_by_css_selector('ul.spaces-list .secondary-sidebar-item .truncate')
        for elem in links:
             if elem.text == name:
                return elem
        return None

    Wait(selenium, WAIT_BACKEND).until(lambda s: find_created_space(selenium).text == name)


@then('user cannot see rename input box')
def wait_to_hide_input_box(selenium):
    Wait(selenium, WAIT_FRONTEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR, 'div#rename-space-modal input')))


@when('user should see, that name was changed successfully')
@then('user should see, that name was changed successfully')
def check_confirmation_after_rename(selenium):
    notify = Wait(selenium, WAIT_FRONTEND).until(
        EC.presence_of_element_located((By.CSS_SELECTOR, '.onedata-notify span.message')))


@then('user can click "Copy" button')
def click_copy_button(selenium):
    copy_button = Wait(selenium, WAIT_FRONTEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, '.input-with-button button.btn'))
    )
    copy_button.click()


@then('user should see invite user token')
def is_invite_user_token_visible(selenium):
    input_box = Wait(selenium, WAIT_FRONTEND).until(
        EC.visibility_of_element_located((By.CSS_SELECTOR, '.input-with-button input#invite-form-token-userJoinSpace-field'))
    )
    text = input_box.get_attribute('value')
    assert len(text) > 0


@then('user should see invite group token')
def is_invite_group_token_visible(selenium):
    input_box = Wait(selenium, WAIT_FRONTEND).until(
        EC.visibility_of_element_located(
            (By.CSS_SELECTOR, '.input-with-button input#invite-form-token-groupJoinSpace-field'))
    )
    text = input_box.get_attribute('value')
    assert len(text) > 0


@then('user should see space creation token')
def is_invite_group_token_visible(selenium):
    input_box = Wait(selenium, WAIT_FRONTEND).until(
        EC.visibility_of_element_located(
            (By.CSS_SELECTOR, '.input-with-button input#invite-form-token-providerSupport-field'))
    )
    text = input_box.get_attribute('value')
    assert len(text) > 0


@then('user should see new url')
def check_if_url_changed(selenium, curr_url):
    assert selenium.current_url != curr_url


@then(parsers.parse('user should see space menu for "{space_name}"'))
def check_space_menu_display(selenium, space_name):

    def get_space_menu(s):
        spaces = s.find_elements_by_css_selector('li.active .secondary-sidebar-item .truncate')
        for elem in spaces:
            if elem.text == space_name:
                return elem
        return None

    Wait(selenium, WAIT_BACKEND).until(get_space_menu)


