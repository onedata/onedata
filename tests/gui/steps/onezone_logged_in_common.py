"""Steps for features of Onezone login page.
"""
from tests.gui.conftest import WAIT_BACKEND
from tests.utils.cucumber_utils import list_parser

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
import re
from pytest_bdd import given, then
from pytest_bdd import parsers
from selenium.webdriver.support.ui import WebDriverWait as wait
from selenium.webdriver.common.keys import Keys
from pytest_bdd import given, when, then, parsers
from tests.gui.utils.generic import parse_url


@when(parsers.parse('I uncollapse {name} main accordion'))
def uncollapse_main_accordion(selenium, name):
    el = selenium.find_element_by_css_selector('a[href="#collapse-{name}"]'.format(name=name))
    aria_expanded = el.get_attribute('aria-expanded')
    if aria_expanded is None or aria_expanded == 'false':
        el.click()


@when('I click on the user alias edit element')
def click_user_alias_edit(selenium):
    selenium.find_element_by_css_selector('.alias-panel a .space-header').click()
    # additional - select all text in active input
    selenium.execute_script('$(".alias-panel a input").select()')


@then('User alias should be changed to "<name>"')
@then(parsers.parse('User alias should be changed to "{name}"'))
def user_alias_equals(selenium, name):
    # TODO: change "space-header" class in op-gui to something more specific
    alias_header = selenium.find_element_by_css_selector('.alias-panel .space-header')
    wait(selenium, WAIT_BACKEND).until(lambda s: alias_header.text == name)

