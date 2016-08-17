"""Steps for features of Oneprovider's spaces.
"""

__author__ = "Michal Cwiertnia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


import selenium

from pytest_bdd import given, parsers, when, then
from tests.gui.steps.common import find_element_by_css_selector_and_text, \
    select_button_from_buttons_by_name
from selenium.webdriver.support.ui import WebDriverWait as Wait
from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By


@when(parsers.parse('user clicks space named "{space_name}" from spaces list'))
def click_space_name(selenium, space_name):
    space_to_click = select_button_from_buttons_by_name(space_name,
                                                        'ul.spaces-list .secondary-sidebar-item')
    Wait(selenium, WAIT_FRONTEND).until(space_to_click).click()


@then(parsers.parse('user should see that home space icon has appeared next to displayed '
                    'name of space "{space_name}" in spaces list'))
def check_if_home_space_icon_next_to_spaces(selenium, space_name):

    def _find_home_space_icon(s):
        spaces = s.find_elements_by_css_selector('.ember-view ul.spaces-list .secondary-sidebar-item')
        for elem in spaces:
            if elem.find_element_by_css_selector('span.oneicon-space-home'):
                return elem
        return None

    assert _find_home_space_icon(selenium).text == space_name


@then(parsers.parse('user should see that submenu for space named "{space_name}" has appeared'))
def check_if_displayed_space_menu(selenium, space_name):
    space = find_element_by_css_selector_and_text('li.active .secondary-sidebar-item .truncate',
                                                  space_name)
    Wait(selenium, WAIT_FRONTEND).until(space)
