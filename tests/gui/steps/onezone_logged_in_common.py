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
from tests.gui.steps.common import find_element_by_css_selector_and_text
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


@when(parsers.parse('user clicks on the "{name}" in "{panel_name}" sidebar panel'))
def click_on_button_in_uncollapsed_oz_panel(selenium, name, panel_name):
    if panel_name.lower() == 'data space management':
        selector = '#collapse-spaces .secondary-header'
    find_button = select_button_from_buttons_by_name(name, selector)
    Wait(selenium, WAIT_FRONTEND).until(find_button).click()


@then(parsers.parse('user should see that the new space has appeared on the '
                    'spaces list in "{panel_name}" sidebar panel'))
def check_spaces_names_headers_whether_new_space_appeared(selenium, name_string):

    def header_with_text_presence(s):
        headers = s.find_elements_by_css_selector('.accordion #collapse-spaces .secondary-header')
        return sum(1 for h in headers if h.text == name_string) == 1

    Wait(selenium, WAIT_BACKEND).until(header_with_text_presence)


@given(parsers.parse('user clicks on the "{name}" provider in Onezone providers sidebar panel'))
def click_on_provider_in_sidebar(selenium, name, clipboard):
    clipboard['supporting_provider'] = name
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


from ..utils.generic import click_on_given_clickable_element


@when('user clicks on the user alias')
def click_user_alias_edit(selenium):
    click_on_given_clickable_element(selenium, item_name='',
                                     css_path='.alias-panel a .space-header',
                                     msg='clicking on user alias in main menu')

    Wait(selenium, WAIT_FRONTEND).until(
        lambda s: s.find_element_by_css_selector('.alias-panel a input')
    ).send_keys(Keys.NULL)
    # selenium.find_element_by_css_selector('.alias-panel a .space-header').click()
    # additional - select all text in active input
    # selenium.execute_script('$(".alias-panel a input").select()')


@when('user clicks on new space name input box')
def click_new_space_name_input_box(selenium):
    Wait(selenium, WAIT_FRONTEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, '#collapse-spaces .secondary-header input'))
    ).click()


@then(parsers.parse('user should see that the alias changed to "{name}"'))
def user_alias_equals(selenium, name):
    alias_header = selenium.find_element_by_css_selector('.accordion .secondary-header')
    Wait(selenium, WAIT_BACKEND).until(lambda s: alias_header.text == name)


@when(parsers.parse('user clicks on the "{button_name}" button from Onezone sidebar panel'))
def click_create_new_space_button(selenium, button_name):
    create_button = find_element_by_css_selector_and_text(selenium, '.secondary-header', button_name)
    Wait(selenium, WAIT_FRONTEND).until(lambda s: create_button is not None)
    create_button.click()
