"""This module contains gherkin steps to run acceptance tests featuring
spaces in oneprovider web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


from pytest_bdd import when, then, parsers

from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed


def _is_space_present_in_sidebar(driver, op_page, space_name):
    spaces = {space.name for space
              in op_page(driver).spaces.sidebar.spaces}
    return space_name in spaces


@when(parsers.parse('user of {browser_id} selects "{space_name}" '
                    'from spaces sidebar list'))
@then(parsers.parse('user of {browser_id} selects "{space_name}" '
                    'from spaces sidebar list'))
def select_sapce_from_sidebar_list(selenium, browser_id, space_name, op_page):
    op_page(selenium[browser_id]).spaces.sidebar.spaces[space_name].click()


@when(parsers.parse('user of {browser_id} sees that space named '
                    '"{name}" has appeared in the spaces list'))
@then(parsers.parse('user of {browser_id} sees that space named '
                    '"{name}" has appeared in the spaces list'))
@repeat_failed(timeout=WAIT_BACKEND, interval=1.5)
def is_present_on_spaces_list(selenium, browser_id, name, op_page):
    driver = selenium[browser_id]
    if not _is_space_present_in_sidebar(driver, op_page, name):
        driver.refresh()
        raise RuntimeError('no space named "{}" found in spaces '
                           'sidebar'.format(name))


@when(parsers.parse('user of {browser_id} does not see "{name}" '
                    'in spaces list'))
@then(parsers.parse('user of {browser_id} does not see "{name}" '
                    'in spaces list'))
@when(parsers.parse('user of {browser_id} sees that space named '
                    '"{name}" has disappeared from the spaces list'))
@then(parsers.parse('user of {browser_id} sees that space named '
                    '"{name}" has disappeared from the spaces list'))
@repeat_failed(timeout=WAIT_BACKEND, interval=1.5)
def is_not_present_in_space_list(selenium, browser_id, name, op_page):
    driver = selenium[browser_id]
    if _is_space_present_in_sidebar(driver, op_page, name):
        driver.refresh()
        raise RuntimeError('space named "{}" found in spaces sidebar, '
                           'while it should not be'.format(name))


@when(parsers.parse('user of {browser_id} clicks on settings icon displayed '
                    'for "{space_name}" item on the spaces sidebar list'))
@then(parsers.parse('user of {browser_id} clicks on settings icon displayed '
                    'for "{space_name}" item on the spaces sidebar list'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_settings_icon_for_space(selenium, browser_id, space_name, op_page):
    (op_page(selenium[browser_id])
     .spaces
     .sidebar
     .spaces[space_name]
     .settings
     .expand())


@when(parsers.parse('user of {browser_id} clicks on the "{option_name}" item '
                    'in settings dropdown for space named "{spaces_name}"'))
@then(parsers.parse('user of {browser_id} clicks on the "{option_name}" item '
                    'in settings dropdown for space named "{spaces_name}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_on_item_in_space_settings_dropdown(selenium, browser_id, option_name,
                                             space_name, op_page):
    (op_page(selenium[browser_id])
     .spaces
     .sidebar
     .spaces[space_name]
     .settings
     .options[option_name]
     .click())


@when(parsers.re('user of (?P<browser_id>.*?) clicks on the '
                 '(?P<btn>Join|Create) button in spaces sidebar header'))
@then(parsers.re('user of (?P<browser_id>.*?) clicks on the '
                 '(?P<btn>Join|Create) button in spaces sidebar header'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_on_btn_in_groups_sidebar_header(selenium, browser_id, btn, op_page):
    getattr(op_page(selenium[browser_id]).spaces.sidebar, btn.lower()).click()


@when(parsers.parse('user of {browser_id} sees that space named "{space_name}" '
                    'is selected one in sidebar spaces list'))
@then(parsers.parse('user of {browser_id} sees that space named "{space_name}" '
                    'is selected one in sidebar spaces list'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_space_is_selected_in_spaces_sidebar(selenium, browser_id,
                                               space_name, op_page):
    assert (op_page(selenium[browser_id])
            .spaces
            .sidebar
            .spaces[space_name]
            .is_selected()) is True, ('"{}" space is not selected while '
                                      'expected to be'.format(space_name))


@when(parsers.parse('user of {browser_id} sees that space named "{space_name}" '
                    'is home space in spaces sidebar list'))
@then(parsers.parse('user of {browser_id} sees that space named "{space_name}" '
                    'is home space in spaces sidebar list'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_space_is_home_in_spaces_sidebar(selenium, browser_id,
                                           space_name, op_page):
    assert (op_page(selenium[browser_id])
            .spaces
            .sidebar
            .spaces[space_name]
            .is_home()) is True, ('"{}" space is not home space while '
                                  'expected to be'.format(space_name))
