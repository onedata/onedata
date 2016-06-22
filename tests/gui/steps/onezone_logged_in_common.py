"""Steps for features of Onezone login page.
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from pytest_bdd import when, then, parsers


@when(parsers.parse('I uncollapse {name} main accordion'))
def uncollapse_main_accordion(selenium, name):
    el = selenium.find_element_by_css_selector('a[href="#collapse-{name}"]'.format(name=name))
    aria_expanded = el.get_attribute('aria-expanded')
    if aria_expanded is None or aria_expanded == 'false':
        el.click()


@when('I click on the user alias edit element')
def click_user_alias_edit(selenium):
    alias_edit = Wait(selenium, WAIT_FRONTEND).until(
        EC.visibility_of_element_located((By.CSS_SELECTOR, '.alias-panel a .space-header'))
    )
    alias_edit.click()
    # selenium.find_element_by_css_selector('.alias-panel a .space-header').click()
    # additional - select all text in active input
    selenium.execute_script('$(".alias-panel a input").select()')


@then('User alias should be changed to "<name>"')
@then(parsers.parse('User alias should be changed to "{name}"'))
def user_alias_equals(selenium, name):
    alias_header = selenium.find_element_by_css_selector('.alias-panel .space-header')
    Wait(selenium, WAIT_BACKEND).until(lambda s: alias_header.text == name)


@when('I go to provider {provider}')
def go_to_provider(selenium, provider):
    providers = selenium.find_elements_by_css_selector('.provider-header')

    def the_provider_is_present(s):
        named_providers = [e for e in providers if e.text == provider]
        if len(named_providers) > 0:
            return named_providers[0]
        else:
            return None

    Wait(selenium, WAIT_FRONTEND).until(the_provider_is_present).click()
    selenium.find_element_by_css_selector('.provider-place-drop a').click()
