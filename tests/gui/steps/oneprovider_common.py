"""Common steps for Oneprovider.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.gui.utils.generic import find_element
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
def get_url(selenium):
    return [selenium.current_url]


@given(parsers.parse('existing {space_name}'))
def existing_space_name(space_name):
    return space_name


@when(parsers.parse('user clicks "{button_name}" button from sidebar panel'))
def click_spaces_button_in_sidebar_panel(selenium, button_name):
    button = find_element(selenium, 'a#main-spaces', button_name)
    button.click()


@then('user should see, that main content has been reloaded')
@when('user should see, that main content has been reloaded')
def check_if_main_content_changed(selenium):
    Wait(selenium, WAIT_FRONTEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR, '.common-loader-spinner'))
    )


@then('user clicks "Create" button from spaces menu bar')
@when('user clicks "Create" button from spaces menu bar')
def click_create_button_in_spaces_menu(selenium):
    create_button = Wait(selenium, WAIT_FRONTEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, 'span.oneicon-space-add')))
    create_button.click()


@then('user should see, that input box for space name is active')
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


@then(parsers.parse('user should see space named "{space_name}" in spaces list'))
@then(parsers.parse('user should see new space named "{space_name}" in spaces list'))
def check_new_space_existence(selenium, space_name):
    Wait(selenium, WAIT_FRONTEND).\
        until(lambda s: find_element(selenium,
                                     'ul.spaces-list .secondary-sidebar-item .truncate',
                                     space_name) is not None)


@then(parsers.parse('user clicks "Settings" icon displayed on space named "{space_name}"'))
@when(parsers.parse('user clicks "Settings" icon displayed on space named "{space_name}"'))
def click_settings_icon_on_space(selenium, space_name):
    space = find_element(selenium, 'ul.spaces-list .secondary-sidebar-item', space_name)
    settings_icon = space.find_element_by_css_selector('span.oneicon-settings')
    settings_icon.click()


@then('user should see settings drop down menu for spaces')
@when('user should see settings drop down menu for spaces')
def wait_for_settings_panel(selenium):

    def _find_expanded_menu(s):
        elems = s.find_elements_by_css_selector('.dropdown-toggle')
        for elem in elems:
            if elem.get_attribute('aria-expanded') == 'true':
                return elem
        return None

    Wait(selenium, WAIT_FRONTEND).until(lambda s: _find_expanded_menu is not None)


@then(parsers.parse('user clicks "{option}" option from drop down menu for spaces'))
@when(parsers.parse('user clicks "{option}" option from drop down menu for spaces'))
def click_option_from_drop_down_menu_for_spaces(selenium, option):
    rename_button = find_element(selenium,
                                 'ul.spaces-list .dropdown .dropdown-menu li.clickable',
                                 option)
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

    def _is_input_box_hidden(s):
        elems = s.find_elements_by_css_selector('#rename-space-backdrop')
        if len(elems) == 0:
            return True
        return False

    Wait(selenium, WAIT_BACKEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR, '#rename-space-backdrop')))
    Wait(selenium, WAIT_FRONTEND).until(_is_input_box_hidden)


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


@then(parsers.parse('user should see, that "{token_name}" token box is not empty'))
def check_existence_of_invite_user_token(selenium, token_name):
    input_box = Wait(selenium, WAIT_FRONTEND).until(
        EC.visibility_of_element_located((By.CSS_SELECTOR,
                                          '.input-with-button input'))
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


@when('user clicks "Join" button from spaces menu')
def click_join_button(selenium):
    join_button = Wait(selenium, WAIT_BACKEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR,'.secondary-sidebar-header figure.icon span.oneicon-join'))
    )
    join_button.click()


@when('user should see, that token input box is active')
def wait_get_input_box_is_active(selenium):

    def _is_active(selenium):
        elem = selenium.find_elements_by_css_selector('#join-space-modal input.form-control')
        if elem:
            elem = elem[0]
            return elem == selenium.switch_to.active_element
        else:
            return False

    Wait(selenium, WAIT_FRONTEND).until(_is_active)


@when('user can see current url')
def set_current_url(selenium, get_url):
    get_url[0] = selenium.current_url


@when(parsers.parse('user clicks space named "{space_name}" from spaces list'))
def click_space_name(selenium, space_name):
    space_to_click = find_element(selenium, 'ul.spaces-list .secondary-sidebar-item', space_name)
    space_to_click.click()


@then(parsers.parse('user should see submenu for space named "{space_name}"'))
def check_if_displayed_space_menu(selenium, space_name):
    Wait(selenium, WAIT_FRONTEND).\
        until(lambda s: find_element(selenium,
                                     'li.active .secondary-sidebar-item .truncate',
                                     space_name) is not None)


@then('user should see that url has changed')
def check_if_url_changed(selenium, get_url):
    assert selenium.current_url != get_url


@then(parsers.parse('user should see home space icon next to "{space_name}"'))
def check_if_home_space_icon_next_to_spaces(selenium, space_name):

    def _find_home_space_icon(s):
        spaces = s.find_elements_by_css_selector('.ember-view ul.spaces-list .secondary-sidebar-item')
        for elem in spaces:
            if elem.find_element_by_css_selector('span.oneicon-space-home'):
                return elem
        return None

    assert _find_home_space_icon(selenium).text == space_name


@when('user clicks "YES" button in popup window asking if he is sure')
def click_yes_button(selenium):
    yes_button = Wait(selenium, WAIT_FRONTEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, '#leave-space-modal form.ember-view button.btn-primary'))
    )
    yes_button.click()


@then('user refreshes site')
def refresh_site(selenium):
    selenium.refresh()


@then(parsers.parse('user should not see space named "{space_name}" in spaces list'))
def check_existance_of_space(selenium, space_name):
    find_space = find_element(selenium, 'ul.spaces-list .secondary-sidebar-item .truncate', space_name)
    assert find_space is None


@then('user should not see popup window')
def wait_to_hide_input_box(selenium):

    def _is_input_box_hidden(s):
        elems = s.find_elements_by_css_selector('#leave-space-backdrop')
        if len(elems) == 0:
            return True
        return False

    Wait(selenium, WAIT_BACKEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR, '#leave-space-backdrop')))
    Wait(selenium, WAIT_FRONTEND).until(_is_input_box_hidden)


####################################################################################################################


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



