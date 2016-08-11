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
from tests.gui.utils.generic import find_element

@when(parsers.parse('user expands the "{name}" Onezone sidebar panel'))
def uncollapse_oz_panel(selenium, name):
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
def uncollapse_oz_panel(selenium, name):
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


@given(parsers.parse('user clicks on the "{name}" provider in Onezone providers sidebar panel'))
def click_on_provider_in_sidebar(selenium, name, get_provider_name):
    collapse_providers = selenium.find_element_by_css_selector('#collapse-providers')

    Wait(selenium, WAIT_FRONTEND).until(lambda s: collapse_providers.get_attribute('aria-expanded') == 'true')

    def the_provider_is_present(s):
        providers = selenium.find_elements_by_css_selector('.provider-header')
        named_providers = [e for e in providers if e.text == name]
        if len(named_providers) > 0:
            return named_providers[0]
        else:
            return None

    get_provider_name = name
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


@when(parsers.parse('user clicks on the "{button_name}" button from Onezone sidebar panel'))
def click_create_new_space_button(selenium, button_name):
    create_button = find_element(selenium, '.secondary-header', button_name)
    Wait(selenium, WAIT_FRONTEND).until(lambda s: create_button is not None)
    create_button.click()


@then(parsers.parse('user should see, that the alias changed to "{name}"'))
def user_alias_equals(selenium, name):
    alias_header = selenium.find_element_by_css_selector('.alias-panel .space-header')
    Wait(selenium, WAIT_BACKEND).until(lambda s: alias_header.text == name)


@then(parsers.parse('user should see new space named "{space_name}" in Onezone sidebar panel'))
def space_name_equals(selenium, space_name):
    space = find_element(selenium, '.secondary-header', space_name)
    Wait(selenium, WAIT_BACKEND).until(lambda s: space is not None)


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
