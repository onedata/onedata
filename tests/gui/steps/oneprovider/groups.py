"""This module contains gherkin steps to run acceptance tests featuring
groups in oneprovider web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


from pytest_bdd import when, then, parsers

from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed


def _is_group_present_in_sidebar(driver, op_page, group_name):
    groups = {group.name for group
              in op_page(driver).groups.sidebar.groups}
    return group_name in groups


@when(parsers.parse('user of {browser_id} selects "{group_name}" '
                    'from groups sidebar list'))
@then(parsers.parse('user of {browser_id} selects "{group_name}" '
                    'from groups sidebar list'))
def select_group_from_sidebar_list(selenium, browser_id, group_name, op_page):
    op_page(selenium[browser_id]).groups.sidebar.groups[group_name].click()


@when(parsers.parse('user of {browser_id} sees that group named '
                    '"{name}" has appeared in the groups list'))
@then(parsers.parse('user of {browser_id} sees that group named '
                    '"{name}" has appeared in the groups list'))
@repeat_failed(timeout=WAIT_BACKEND, interval=1.5)
def is_present_on_groups_list(selenium, browser_id, name, op_page):
    driver = selenium[browser_id]
    if not _is_group_present_in_sidebar(driver, op_page, name):
        driver.refresh()
        raise RuntimeError('no group named "{}" found in groups '
                           'sidebar'.format(name))


@when(parsers.parse('user of {browser_id} does not see "{name}" '
                    'in groups list'))
@then(parsers.parse('user of {browser_id} does not see "{name}" '
                    'in groups list'))
@when(parsers.parse('user of {browser_id} sees that group named '
                    '"{name}" has disappeared from the groups list'))
@then(parsers.parse('user of {browser_id} sees that group named '
                    '"{name}" has disappeared from the groups list'))
@repeat_failed(timeout=WAIT_BACKEND, interval=1.5)
def is_not_present_in_group_list(selenium, browser_id, name, op_page):
    driver = selenium[browser_id]
    if _is_group_present_in_sidebar(driver, op_page, name):
        driver.refresh()
        raise RuntimeError('group named "{}" found in groups sidebar, '
                           'while it should not be'.format(name))


@when(parsers.parse('user of {browser_id} clicks on settings icon displayed '
                    'for "{group_name}" item on the groups sidebar list'))
@then(parsers.parse('user of {browser_id} clicks on settings icon displayed '
                    'for "{group_name}" item on the groups sidebar list'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_settings_icon_for_group(selenium, browser_id, group_name, op_page):
    (op_page(selenium[browser_id])
     .groups
     .sidebar
     .groups[group_name]
     .settings
     .expand())


@when(parsers.parse('user of {browser_id} clicks on the "{option_name}" item '
                    'in settings dropdown for group named "{group_name}"'))
@then(parsers.parse('user of {browser_id} clicks on the "{option_name}" item '
                    'in settings dropdown for group named "{group_name}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_on_item_in_group_settings_dropdown(selenium, browser_id, option_name,
                                             group_name, op_page):
    (op_page(selenium[browser_id])
     .groups
     .sidebar
     .groups[group_name]
     .settings
     .options[option_name]
     .click())


@when(parsers.re('user of (?P<browser_id>.*?) clicks on the '
                 '(?P<btn>Join|Create) button in groups sidebar header'))
@then(parsers.re('user of (?P<browser_id>.*?) clicks on the '
                 '(?P<btn>Join|Create) button in groups sidebar header'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_on_btn_in_groups_sidebar_header(selenium, browser_id, btn, op_page):
    getattr(op_page(selenium[browser_id]).groups.sidebar, btn.lower()).click()


@when(parsers.re(r'user of (?P<browser_id>.*) sees that "(?P<name>.*)" item '
                 r'has appeared on current (?P<caption>USERS|GROUPS) '
                 r'permissions table in Groups tab'))
@then(parsers.re(r'user of (?P<browser_id>.*) sees that "(?P<name>.*)" item '
                 r'has appeared on current (?P<caption>USERS|GROUPS) '
                 r'permissions table in Groups tab'))
@repeat_failed(timeout=WAIT_BACKEND, interval=1.5)
def assert_item_appeared_in_groups_perm_table(selenium, browser_id, name,
                                              caption, op_page):
    driver = selenium[browser_id]
    items = getattr(op_page(driver).groups.permission_table, caption.lower())
    items_names = {item.name for item in items}
    if name not in items_names:
        driver.refresh()
        raise RuntimeError('no {} named "{}" found in spaces permission table'
                           ''.format(caption, name))
