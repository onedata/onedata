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
    return space_name


@when('user clicks "Spaces" button')
def click_spaces_button(selenium):
    spaces_button = Wait(selenium, WAIT_FRONTEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, 'a#main-spaces')))
    spaces_button.click()


@when('user clicks "Create" button')
def click_create_button(selenium):
    create_button = Wait(selenium, WAIT_BACKEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, 'span.oneicon-space-add')))
    create_button.click()


@when('user should see, that input box for space name is active')
def wait_input_box_for_space_name_is_active(selenium):

    def _is_active(selenium):
        elem = selenium.find_elements_by_css_selector('div#create-space-modal input')
        if elem:
            elem = elem[0]
            return elem == selenium.switch_to.active_element
        else:
            return False

    Wait(selenium, WAIT_FRONTEND).until(_is_active)


@then(parsers.parse('user should see space named "{name}"'))
@then(parsers.parse('user should see new space named "{name}"'))
def new_space_existance(selenium, name):

    def find_created_space(s):
        links = s.find_elements_by_css_selector('ul.spaces-list .secondary-sidebar-item .truncate')
        for elem in links:
             if elem.text == name:
                return elem
        return None

    Wait(selenium, WAIT_BACKEND).until(lambda s: find_created_space(selenium).text == name)


@then(parsers.parse('click "Settings" icon in "{space}"'))
@when(parsers.parse('click "Settings" icon in "{space}"'))
def click_settings_icon_in_space(selenium, space):

    def find_space(s):
        links = s.find_elements_by_css_selector('ul.spaces-list .secondary-sidebar-item')
        for elem in links:
             if elem.text == space:
                return elem.find_element_by_css_selector('span.oneicon-settings')
        return None

    find_space(selenium).click()


@then(parsers.parse('user clicks "{option}" option'))
@when(parsers.parse('user clicks "{option}" option'))
def click_rename_option(selenium, option):

    def find_option_button(selenium):
        links = selenium.find_elements_by_css_selector('ul.spaces-list .dropdown .dropdown-menu li.clickable')
        for elem in links:
            if elem.text == option:
                return elem
        return None

    rename_button = Wait(selenium, WAIT_FRONTEND).until(find_option_button)
    rename_button.click()


@then('user should see, that input box for new name is active')
@when('user should see, that input box for new name is active')
def wait_input_box_for_new_name_is_active(selenium):

    def _is_active(selenium):
        elem = selenium.find_elements_by_css_selector('div#rename-space-modal input')
        if elem:
            elem = elem[0]
            return elem == selenium.switch_to.active_element
        else:
            return False

    Wait(selenium, WAIT_FRONTEND).until(_is_active)


@then('user should not see input box for new name')
def wait_to_hide_input_box(selenium):
    Wait(selenium, WAIT_FRONTEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR, 'div#rename-space-modal input')))
    Wait(selenium, WAIT_FRONTEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR, '#rename-space-backdrop')))


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


@then('user should see invite user token')
def check_existence_of_invite_user_token(selenium):
    input_box = Wait(selenium, WAIT_FRONTEND).until(
        EC.visibility_of_element_located((By.CSS_SELECTOR, '.input-with-button input#invite-form-token-userJoinSpace-field'))
    )
    text = input_box.get_attribute('value')
    assert len(text) > 0


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


@then('user should see invite group token')
def check_exitence_of_invite_group_token(selenium):
    input_box = Wait(selenium, WAIT_FRONTEND).until(
        EC.visibility_of_element_located(
            (By.CSS_SELECTOR, '.input-with-button input#invite-form-token-groupJoinSpace-field'))
    )
    text = input_box.get_attribute('value')
    assert len(text) > 0


@when('user should see, that get support token box is active')
def wait_get_support_token_input_box_is_active(selenium):

    def _is_active(selenium):
        elem = selenium.find_elements_by_css_selector('.input-with-button input#invite-form-token-providerSupport-field')
        if elem:
            elem = elem[0]
            return elem == selenium.switch_to.active_element
        else:
            return False

    Wait(selenium, WAIT_FRONTEND).until(_is_active)


@then('user should see get support token')
def check_exitence_of_get_support_token(selenium):
    input_box = Wait(selenium, WAIT_FRONTEND).until(
        EC.visibility_of_element_located(
            (By.CSS_SELECTOR, '.input-with-button input#invite-form-token-providerSupport-field'))
    )
    text = input_box.get_attribute('value')
    assert len(text) > 0


@when('user clicks "Join" button')
def click_join_button(selenium):
    join_button = Wait(selenium, WAIT_BACKEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR,'.secondary-sidebar-header figure.icon span.oneicon-join'))
    )
    join_button.click()


@when('user sees current url')
def set_current_url(selenium, curr_url):
    curr_url[0] = selenium.current_url


@when(parsers.parse('user clicks "{space_name}"'))
def click_space_name(selenium, space_name):

    def find_space_to_click(s):
        spaces = s.find_elements_by_css_selector('ul.spaces-list .secondary-sidebar-item')
        for elem in spaces:
            if elem.text == space_name:
                return elem
        return None

    Wait(selenium, WAIT_BACKEND).until(find_space_to_click).click()


@then(parsers.parse('user should see space menu for "{space_name}"'))
def check_if_displayed_space_menu(selenium, space_name):

    def find_space_menu(s):
        spaces = s.find_elements_by_css_selector('li.active .secondary-sidebar-item .truncate')
        for elem in spaces:
            if elem.text == space_name:
                return elem
        return None

    Wait(selenium, WAIT_BACKEND).until(find_space_menu)


@then('user should see new url')
def check_if_url_changed(selenium, curr_url):
    assert selenium.current_url != curr_url


@then(parsers.parse('user should see home space icon next to "{space_name}"'))
def check_if_home_space_icon_next_to_spaces(selenium, space_name):

    def find_home_space_icon(s):
        spaces = s.find_elements_by_css_selector('.ember-view ul.spaces-list .secondary-sidebar-item')
        for elem in spaces:
            if elem.find_element_by_css_selector('span.oneicon-space-home'):
                return elem
        return None

    assert find_home_space_icon(selenium).text == space_name


@when('user clicks "YES" button')
def click_yes_button(selenium):
    yes_button = Wait(selenium, WAIT_FRONTEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, '#leave-space-modal form.ember-view button.btn-primary'))
    )
    yes_button.click()


@then('user refreshes site')
def refresh_site(selenium):
    selenium.refresh()


@then(parsers.parse('user should not see space named "{space_name}"'))
def check_existance_of_space(selenium, space_name):

    def try_find_space(s):
        links = s.find_elements_by_css_selector('ul.spaces-list .secondary-sidebar-item .truncate')
        for elem in links:
             if elem.text == space_name:
                return elem
        return None

    assert try_find_space(selenium) is None

####################################################################################################################
@then('user sees settings dropdown panel')
@when('user sees settings dropdown panel')
def wait_for_settings_panel(selenium):
    Wait(selenium, WAIT_BACKEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, '.settings-dropdown ul.dropdown-menu-list span.item-label'))
    )


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




#@when('user should see, that name was changed successfully')
#@then('user should see, that name was changed successfully')
#def check_confirmation_after_rename(selenium):
#    notify = Wait(selenium, WAIT_FRONTEND).until(
 #       EC.presence_of_element_located((By.CSS_SELECTOR, '.onedata-notify span.message')))


@then('user can click "Copy" button')
def click_copy_button(selenium):
    copy_button = Wait(selenium, WAIT_FRONTEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, '.input-with-button button.btn'))
    )
    copy_button.click()



