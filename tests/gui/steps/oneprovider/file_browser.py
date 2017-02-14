"""Steps used for file list handling in various GUI testing scenarios
"""

from datetime import datetime

import pytest
from pytest_bdd import when, then, parsers
from pytest_selenium_multi import select_browser

from tests.gui.conftest import WAIT_BACKEND, SELENIUM_IMPLICIT_WAIT
from tests.gui.utils.generic import parse_seq, repeat, implicit_wait

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@when(parsers.parse('user of {browser_id} sees "{msg}" '
                    'instead of file browser'))
@then(parsers.parse('user of {browser_id} sees "{msg}" '
                    'instead of file browser'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def assert_msg_instead_of_browser(browser_id, msg, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    displayed_msg = browser.empty_dir_msg
    assert displayed_msg == msg, 'displayed {} does not match expected ' \
                                 '{}'.format(displayed_msg, msg)


@when(parsers.parse('user of {browser_id} sees {tool_type} icon for '
                    '{item_type} named "{item_name}"'))
@then(parsers.parse('user of {browser_id} sees {tool_type} icon for '
                    '{item_type} named "{item_name}"'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def assert_tool_icon_for_file_in_file_browser(browser_id, tool_type,
                                              item_name, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    err_msg = '{} tool for {} in file browser visible, ' \
              'while should not be'.format(tool_type, item_name)
    assert not browser[item_name].is_tool_visible(tool_type), err_msg


@when(parsers.parse('user of {browser_id} sees {tool_type} icon for '
                    '{item_type} named "{item_name}"'))
@then(parsers.parse('user of {browser_id} sees {tool_type} icon for '
                    '{item_type} named "{item_name}"'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def assert_tool_icon_for_file_in_file_browser(browser_id, tool_type,
                                              item_name, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    err_msg = '{} tool for {} in file browser not visible'.format(tool_type,
                                                                  item_name)
    assert browser[item_name].is_tool_visible(tool_type), err_msg


@when(parsers.parse('user of {browser_id} clicks on {tool_type} '
                    'tool icon in file row for item named "{item_name}" '
                    'in file browser'))
@then(parsers.parse('user of {browser_id} clicks on {tool_type} '
                    'tool icon in file row for item named "{item_name}" '
                    'in file browser'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def click_on_tool_icon_for_file_in_file_browser(browser_id, tool_type,
                                                item_name, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    browser[item_name].click_on_tool(tool_type)


@when(parsers.parse('user of {browser_id} sees that item(s) named {item_list} '
                    'has(have) disappeared from files browser'))
@then(parsers.parse('user of {browser_id} sees that item(s) named {item_list} '
                    'has(have) disappeared from files browser'))
@when(parsers.parse('user of {browser_id} does not see any item(s) named '
                    '{item_list} in file browser'))
@then(parsers.parse('user of {browser_id} does not see any item(s) named '
                    '{item_list} in file browser'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def assert_items_absence_in_file_browser(selenium, browser_id, item_list,
                                         tmp_memory):
    driver = select_browser(selenium, browser_id)
    browser = tmp_memory[browser_id]['file_browser']
    with implicit_wait(driver, 0.1, SELENIUM_IMPLICIT_WAIT):
        for item_name in parse_seq(item_list):
            with pytest.raises(RuntimeError):
                _ = browser[item_name]


@when(parsers.parse('user of {browser_id} sees item(s) '
                    'named {item_list} in file browser'))
@then(parsers.parse('user of {browser_id} sees item(s) '
                    'named {item_list} in file browser'))
@when(parsers.parse('user of {browser_id} sees that item(s) named '
                    '{item_list} has(have) appeared in file browser'))
@then(parsers.parse('user of {browser_id} sees that item(s) named '
                    '{item_list} has(have) appeared in file browser'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def assert_items_presence_in_file_browser(browser_id, item_list, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    for item_name in parse_seq(item_list):
        _ = browser[item_name]


@when(parsers.parse('user of {browser_id} sees item(s) named '
                    '{item_list} in file browser in given order'))
@then(parsers.parse('user of {browser_id} sees item(s) named '
                    '{item_list} in file browser in given order'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def assert_presence_in_file_browser_with_order(browser_id, item_list,
                                               tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    items = list(reversed(parse_seq(item_list)))
    for item in browser:
        if item.name == items[-1]:
            items.pop()
    assert items == [], 'item(s) not in browser or not in given order ' \
                        'starting from {}'.format(list(reversed(items)))


@when(parsers.parse('user of {browser_id} sees that modification date of item '
                    'named "{item_name}" is {date} with possible error of '
                    '{err_time:d} seconds in file browser'))
@then(parsers.parse('user of {browser_id} sees that modification date of item '
                    'named "{item_name}" is {date} with possible error of '
                    '{err_time:d} seconds in file browser'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def assert_item_in_file_browser_is_of_size(browser_id, item_name, date,
                                           err_time, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    date_fmt = '%Y-%m-%d %H:%M'
    item_date = datetime.strptime(browser[item_name].modification_date, date_fmt)
    expected_date = datetime.strptime(date, date_fmt)
    err_msg = 'displayed mod time {} for {} does not match expected {}'
    assert abs(expected_date - item_date).seconds < err_time, \
        err_msg.format(item_date, item_name, expected_date)


@when(parsers.parse('user of {browser_id} sees that item named "{item_name}" '
                    'is of {size} size in file browser'))
@then(parsers.parse('user of {browser_id} sees that item named "{item_name}" '
                    'is of {size} size in file browser'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def assert_item_in_file_browser_is_of_size(browser_id, item_name, size,
                                           tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    item_size = browser[item_name].size
    err_msg = 'displayed size {} for {} does not match expected {}'
    assert size == item_size, err_msg.format(item_size, item_name, size)


@when(parsers.parse('user of {browser_id} scrolls to the bottom '
                    'of file browser'))
@then(parsers.parse('user of {browser_id} scrolls to the bottom '
                    'of file browser'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def scroll_to_bottom_of_file_browser(browser_id, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    browser.scroll_to_bottom()


@when(parsers.parse('user of {browser_id} sees that there is(are) {num:d} '
                    'item(s) in file browser'))
@then(parsers.parse('user of {browser_id} sees that there is(are) {num:d} '
                    'item(s) in file browser'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def assert_num_of_files_are_displayed_in_file_browser(browser_id, num,
                                                      tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    err_msg = 'displayed number of files {} does not match expected {}'
    files_num = browser.files_count
    assert browser.files_count == num, err_msg.format(files_num, num)


@when(parsers.parse('user of {browser_id} sees that item named "{item_name}" '
                    'is {item_attr} in file browser'))
@then(parsers.parse('user of {browser_id} sees that item named "{item_name}" '
                    'is {item_attr} in file browser'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def assert_item_in_file_browser_is_of_type(browser_id, item_name, item_attr,
                                           tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    action = getattr(browser[item_name], 'is_{}'.format(item_attr))
    assert action(), '"{}" is not {}, while it should'.format(item_name,
                                                              item_attr)


@when(parsers.parse('user of {browser_id} double clicks on item '
                    'named "{item_name}" in file browser'))
@then(parsers.parse('user of {browser_id} double clicks on item '
                    'named "{item_name}" in file browser'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def double_click_on_item_in_file_browser(browser_id, item_name, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    browser[item_name].double_click()


@when(parsers.parse('user of {browser_id} clicks once on item '
                    'named "{item_name}" in file browser'))
@then(parsers.parse('user of {browser_id} clicks once on item '
                    'named "{item_name}" in file browser'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def click_on_item_in_file_browser(browser_id, item_name, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    browser[item_name].click()


@when(parsers.parse('user of {browser_id} selects {item_list} '
                    'item(s) from file browser with pressed shift'))
@then(parsers.parse('user of {browser_id} selects {item_list} '
                    'item(s) from file browser with pressed shift'))
def select_files_from_file_list_using_shift(browser_id, item_list, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    with browser.select_files() as selector:
        selector.shift_down()
        _select_files(browser, selector, item_list)
        selector.shift_up()


@when(parsers.parse('user of {browser_id} selects {item_list} '
                    'item(s) from file browser with pressed ctrl'))
@then(parsers.parse('user of {browser_id} selects {item_list} '
                    'item(s) from file browser with pressed ctrl'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def select_files_from_file_list_using_ctrl(browser_id, item_list,
                                           tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    with browser.select_files() as selector:
        selector.ctrl_down()
        _select_files(browser, selector, item_list)
        selector.ctrl_up()


@when(parsers.parse('user of {browser_id} deselects {item_list} '
                    'item(s) from file browser'))
@then(parsers.parse('user of {browser_id} deselects {item_list} '
                    'item(s) from file browser'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def deselect_items_from_file_browser(browser_id, item_list, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    with browser.select_files() as selector:
        selector.ctrl_down()
        _deselect_files(browser, selector, item_list)
        selector.ctrl_up()


def _select_files(browser, selector, item_list):
    for item_name in parse_seq(item_list):
        item = browser[item_name]
        if not item.is_selected():
            selector.select(item)


def _deselect_files(browser, selector, item_list):
    for item_name in parse_seq(item_list):
        item = browser[item_name]
        if item.is_selected():
            selector.select(item)


@when(parsers.parse('user of {browser_id} deselects all '
                    'selected items from file browser'))
@then(parsers.parse('user of {browser_id} deselects all '
                    'selected items from file browser'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def deselect_all_items_from_file_browser(browser_id, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    item = browser[0]
    item.click()
    if item.is_selected():
        item.click()


@when(parsers.parse('user of {browser_id} sees that {item_list} '
                    'item(s) is(are) selected in file browser'))
@then(parsers.parse('user of {browser_id} sees that {item_list} '
                    'item(s) is(are) selected in file browser'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def assert_items_are_selected_in_file_browser(browser_id, item_list,
                                              tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    err_msg = 'item "{name}" is not selected while it should be'
    for item_name in parse_seq(item_list):
        item = browser[item_name]
        assert item.is_selected(), err_msg.format(name=item_name)


@when(parsers.parse('user of {browser_id} sees that {item_list} '
                    'item(s) is(are) not selected in file browser'))
@then(parsers.parse('user of {browser_id} sees that {item_list} '
                    'item(s) is(are) not selected in file browser'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def assert_items_are_not_selected_in_file_browser(browser_id, item_list,
                                                  tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    err_msg = 'item "{name}" is selected while it should not be'
    for item_name in parse_seq(item_list):
        item = browser[item_name]
        assert not item.is_selected(), err_msg.format(name=item_name)


@when(parsers.parse('user of {browser_id} sees that none '
                    'item is selected in file browser'))
@then(parsers.parse('user of {browser_id} sees that none '
                    'item is selected in file browser'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def assert_none_item_is_selected_in_file_browser(browser_id, item_list,
                                                 tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    err_msg = 'item "{name}" is selected while it should not be'
    for item_name in parse_seq(item_list):
        item = browser[item_name]
        assert not item.is_selected(), err_msg.format(name=item_name)
