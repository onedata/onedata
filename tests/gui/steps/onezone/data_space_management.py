"""Steps for DATA SPACE MANAGEMENT panel features of Onezone page.
"""

import pytest
from pytest_bdd import parsers, when, then
from pytest_selenium_multi.pytest_selenium_multi import select_browser

from tests.gui.conftest import WAIT_BACKEND
from tests.gui.utils.generic import repeat_failed

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@when(parsers.parse('user of {browser_id} clicks on "Create new space" '
                    'button in expanded "ACCESS TOKENS" Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on "Create new space" '
                    'button in expanded "ACCESS TOKENS" Onezone panel'))
def click_on_create_new_space_in_oz_panel(selenium, browser_id, oz_page):
    driver = select_browser(selenium, browser_id)
    oz_page(driver)['data space management'].create_new_space()


@when(parsers.parse('user of {browser_id} clicks on "Create new space" '
                    'button in expanded "ACCESS TOKENS" Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on "Create new space" '
                    'button in expanded "ACCESS TOKENS" Onezone panel'))
def click_on_join_space_in_oz_panel(selenium, browser_id, oz_page):
    driver = select_browser(selenium, browser_id)
    oz_page(driver)['data space management'].join_space()


@when(parsers.parse('user of {browser_id} focuses on activated edit box for '
                    'creating new space in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
@then(parsers.parse('user of {browser_id} focuses on activated edit box for '
                    'creating new space in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
def get_create_space_edit_box(selenium, browser_id, tmp_memory, oz_page):
    driver = select_browser(selenium, browser_id)
    edit_box = oz_page(driver)['data space management'].create_space_edit_box
    tmp_memory[browser_id]['edit_box'] = edit_box


@when(parsers.parse('user of {browser_id} sees that space named "{name}" has '
                    'disappeared from expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@then(parsers.parse('user of {browser_id} sees that space named "{name}" has '
                    'disappeared from expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@when(parsers.parse('user of {browser_id} sees that there is no space named '
                    '"{name}" in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@then(parsers.parse('user of {browser_id} sees that there is no space named '
                    '"{name}" in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
def assert_there_is_no_space_named_in_panel(selenium, browser_id, name, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def assert_space_not_exist(d, space_name):
        with pytest.raises(RuntimeError):
            # noinspection PyStatementEffect
            oz_page(d)['data space management'][space_name]

    assert_space_not_exist(driver, name)


@when(parsers.parse('user of {browser_id} sees that space named "{name}" has '
                    'appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@then(parsers.parse('user of {browser_id} sees that space named "{name}" has '
                    'appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
def assert_there_is_space_named_in_panel(selenium, browser_id, name, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def assert_space_exist(d, space_name):
        # noinspection PyStatementEffect
        oz_page(d)['data space management'][space_name]

    assert_space_exist(driver, name)


@when(parsers.parse('user of {browser_id} expands settings dropdown for space '
                    'named "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel by clicking on settings icon'))
@then(parsers.parse('user of {browser_id} expands settings dropdown for space '
                    'named "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel by clicking on settings icon'))
def expand_settings_dropdown_for_space_in_panel(selenium, browser_id,
                                                name, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def expand_settings_dropdown(d, space_name):
        oz_page(d)['data space management'][space_name].settings.expand()

    expand_settings_dropdown(driver, name)


@when(parsers.re('user of (?P<browser_id>.+?) clicks on the '
                 '"(?P<option>LEAVE|RENAME|GET SUPPORT|SET AS HOME)" item in '
                 'settings dropdown for space named "(?P<name>.+?)" '
                 'in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@then(parsers.re('user of (?P<browser_id>.+?) clicks on the '
                 '"(?P<option>LEAVE|RENAME|GET SUPPORT|SET AS HOME)" item in '
                 'settings dropdown for space named "(?P<name>.+?)" '
                 'in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
def click_on_settings_option_for_space_in_panel(selenium, browser_id,
                                                option, name, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def click_on_settings_item(d, space_name, chosen_option):
        settings = oz_page(d)['data space management'][space_name].settings
        getattr(settings, chosen_option)()

    click_on_settings_item(driver, name, option.lower().replace(' ', '_'))


