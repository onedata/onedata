"""Steps for features of Onezone login page.
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from pytest_bdd import given, when, then, parsers
from selenium.webdriver.common.keys import Keys

from common import select_button_from_buttons_by_name


def _uncollapse_oz_panel(selenium, name):
    re_lc_name = re.compile(name, re.I)

    def sidebar_group_by_name(s):
        groups = s.find_elements_by_css_selector('.main-accordion-group')
        for g in groups:
            t = g.find_element_by_css_selector('a.main-accordion-toggle')
            if re_lc_name.match(t.text):
                return g, t
        return None

    sgroup, toggle = Wait(selenium, WAIT_FRONTEND).until(sidebar_group_by_name)
    aria_expanded = sgroup.get_attribute('aria-expanded')
    if aria_expanded is None or aria_expanded == 'false':
        toggle.click()


@given(parsers.parse('user expands the "{name}" Onezone sidebar panel'))
def g_uncollapse_oz_panel(selenium, name):
    _uncollapse_oz_panel(selenium, name)


@when(parsers.parse('user expands the "{name}" Onezone sidebar panel'))
def w_uncollapse_oz_panel(selenium, name):
    _uncollapse_oz_panel(selenium, name)


@given(parsers.parse('user clicks on the "{name}" Oneprovider\'s sidebar panel'))
def open_op_panel(selenium, name):
    css_selector = '.primary-sidebar a#main-' + name
    find_button = select_button_from_buttons_by_name(name, css_selector)
    Wait(selenium, WAIT_FRONTEND).until(find_button).click()



@given('user has new name for group')
@given('user has name for new group')
@given('user has name for new space')
def random_name():
    import random
    chars = 'qwertyuioplkjhgfdsazxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890'
    return ''.join(random.sample(chars, 6))


@when(parsers.parse('user types group name on keyboard'))
@when(parsers.parse('user types space name on keyboard'))
def type_string_into_active_element(selenium, random_name):
    selenium.switch_to.active_element.send_keys(random_name)


@when('user types new group name on keyboard')
def clear_and_type_string_into_active_element(selenium, random_name):
    selenium.switch_to.active_element.clear()
    selenium.switch_to.active_element.send_keys(random_name)


@when('user types "{name}" on keyboard')
def clear_and_type_string_into_active_element(selenium, name):
    selenium.switch_to.active_element.clear()
    selenium.switch_to.active_element.send_keys(name)


@when(parsers.parse('user clicks on the "{name}"'))
def w1_click_on_button(selenium, name):
    def go_to_button(s):
        links = s.find_elements_by_css_selector('div.secondary-header')
        for e in links:
            if e.text == name:
                return e

    Wait(selenium, WAIT_FRONTEND).until(go_to_button).click()


@then(parsers.parse('user should see, that the new space appear on the list'))
def page_with_header(selenium, random_name):

    def header_with_text_presence(s):
        headers = s.find_elements_by_css_selector('div.secondary-header')
        return any(h.text == random_name for h in headers)

    Wait(selenium, WAIT_BACKEND).until(header_with_text_presence)


@when(parsers.parse('user clicks on the "{name}" button'))
def w2_click_on_button(selenium, name):
    def go_to_button(s):
        links = s.find_elements_by_css_selector('figure.icon')
        for e in links:
            if e.text == name:
                return e

    Wait(selenium, WAIT_FRONTEND).until(go_to_button).click()


@then(parsers.parse('user should see, that the new group appear on the list'))
def page_with_header(selenium, random_name):

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
            elem_name = group.find_element_by_css_selector('.item-label '
                                                           '.truncate').text
            if name == elem_name:
                return group.find_element_by_css_selector('.dropdown'
                                                          '.settings-dropdown')

    Wait(selenium, WAIT_FRONTEND).until(_locate_settings_button).click()


@when(parsers.parse('user clicks on the "{elem}" element'))
def click_on_elem(selenium, elem):
    def go_to_button(s):
        elems = s.find_elements_by_css_selector('.settings-dropdown '
                                                '.dropdown-menu-settings '
                                                '.clickable')
        for e in elems:
            if elem == e.text:
                return e

    Wait(selenium, WAIT_FRONTEND).until(go_to_button).click()


@then('user should see popup with information about name change')
def check_confirmation_after_rename(selenium, name, random_name):
    from common import notify_visible_with_text
    notify_visible_with_text(selenium, "info", '.*' + name + '.*renamed.*' + random_name + '.*')


@then(parsers.parse('user should see, that the new name replaced old one on the list'))
def renamed_group(selenium, name, random_name):

    def header_with_text_presence(s):
        headers = s.find_elements_by_css_selector('.groups-list .secondary-sidebar-item .item-label .truncate')
        return all(h.text != name for h in headers) and any(h.text == random_name for h in headers)

    Wait(selenium, WAIT_BACKEND).until(header_with_text_presence)

    # restore previous name
    click_on_settings_button_in_group_panel(selenium, random_name)
    click_on_elem(selenium, "RENAME")
    wait_for_rename_input_box(selenium)
    clear_and_type_string_into_active_element(selenium, name)
    selenium.switch_to.active_element.send_keys(Keys.RETURN)

# sprawdzanie file dist czy jest ten prov na ktorego sie weszlo






@given(parsers.parse('user clicks on the "{name}" provider in Onezone providers sidebar panel'))
def click_on_provider_in_sidebar(selenium, name):
    collapse_providers = selenium.find_element_by_css_selector('#collapse-providers')

    Wait(selenium, WAIT_FRONTEND).until(lambda s: collapse_providers.get_attribute('aria-expanded') == 'true')

    def the_provider_is_present(s):
        providers = selenium.find_elements_by_css_selector('.provider-header')
        named_providers = [e for e in providers if e.text == name]
        if len(named_providers) > 0:
            return named_providers[0]
        else:
            return None

    Wait(selenium, WAIT_FRONTEND).until(the_provider_is_present).click()


@given(parsers.parse('user clicks on the "Go to your files" button in provider popup'))
def click_on_go_to_files_provider(selenium):
    def go_to_files_button(s):
        links = s.find_elements_by_css_selector('.provider-place-drop a, .provider-place-drop button')
        for e in links:
            if e.text == 'Go to your files':
                return e

    Wait(selenium, WAIT_FRONTEND).until(go_to_files_button).click()


@when('user clicks on the user alias')
def click_user_alias_edit(selenium):
    alias_edit = Wait(selenium, WAIT_FRONTEND).until(
        EC.visibility_of_element_located((By.CSS_SELECTOR, '.alias-panel a .space-header'))
    )
    alias_edit.click()
    # selenium.find_element_by_css_selector('.alias-panel a .space-header').click()
    # additional - select all text in active input
    selenium.execute_script('$(".alias-panel a input").select()')


@then(parsers.parse('user should see, that the alias changed to "helloworld"'))
def user_alias_equals(selenium, name):
    alias_header = selenium.find_element_by_css_selector('.alias-panel .space-header')
    Wait(selenium, WAIT_BACKEND).until(lambda s: alias_header.text == name)


# @when('I go to provider {provider}')
# def go_to_provider(selenium, provider):
#     providers = selenium.find_elements_by_css_selector('.provider-header')
#
#     def the_provider_is_present(s):
#         named_providers = [e for e in providers if e.text == provider]
#         if len(named_providers) > 0:
#             return named_providers[0]
#         else:
#             return None
#
#     Wait(selenium, WAIT_FRONTEND).until(the_provider_is_present).click()
#     selenium.find_element_by_css_selector('.provider-place-drop a').click()
