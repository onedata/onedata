"""Steps for features of Onezone login page.
"""

from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND
from tests.gui.utils.generic import repeat_failed
from tests.utils.acceptance_utils import list_parser

from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.common.keys import Keys

from pytest_bdd import given, when, then, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser


__author__ = "Jakub Liput, Bartek Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


panel_to_css = {'access tokens': '#collapse-tokens',
                'go to your files': '#collapse-providers',
                'data space management': '#collapse-spaces',
                'user alias': '#collapse-alias',
                'authentication settings': '#collapse-accounts'}


icon_to_css = {'create space': 'oneicon-space-add'}


def _oz_expand_oz_panel(oz_page, driver, panel_name):
    oz_page(driver)[panel_name].expand()


@given(parsers.re('users? of (?P<browser_id_list>.*) expanded the '
                  '"(?P<panel_name>.*)" Onezone sidebar panel'))
def g_expand_oz_panel(selenium, browser_id_list, panel_name, oz_page):
    for browser_id in list_parser(browser_id_list):
        driver = select_browser(selenium, browser_id)
        _oz_expand_oz_panel(oz_page, driver, panel_name)


@when(parsers.re('users? of (?P<browser_id_list>.*) expands? the '
                 '"(?P<panel_name>.*)" Onezone sidebar panel'))
@then(parsers.re('users? of (?P<browser_id_list>.*) expands? the '
                 '"(?P<panel_name>.*)" Onezone sidebar panel'))
def wt_expand_oz_panel(selenium, browser_id_list, panel_name, oz_page):
    for browser_id in list_parser(browser_id_list):
        driver = select_browser(selenium, browser_id)
        _oz_expand_oz_panel(oz_page, driver, panel_name)


@when(parsers.parse('user of {browser_id} clicks on {btn} button displayed '
                    'next to active edit box'))
@then(parsers.parse('user of {browser_id} clicks on {btn} button displayed '
                    'next to active edit box'))
def click_on_btn_for_edit_box(browser_id, btn, tmp_memory):
    edit_box = tmp_memory[browser_id]['edit_box']
    if btn == 'confirm':
        edit_box.confirm_input()
    elif btn == 'cancel':
        edit_box.cancel_input()
    else:
        raise RuntimeError('unrecognized edit box btn: {}'.format(btn))


@when(parsers.parse('user of {browser_id} types "{text}" to active edit box'))
@then(parsers.parse('user of {browser_id} types "{text}" to active edit box'))
def type_text_into_active_edit_box(browser_id, text, tmp_memory):
    edit_box = tmp_memory[browser_id]['edit_box']
    edit_box.value = text


@when(parsers.re('user of (?P<browser_id>.+?) sees that there is '
                 '(?P<item_type>provider) named "(?P<item_name>.+?)" '
                 'in expanded "(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that there is '
                 '(?P<item_type>provider) named "(?P<item_name>.+?)" '
                 'in expanded "(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@when(parsers.re('user of (?P<browser_id>.+?) sees that (?P<item_type>provider) '
                 'named "(?P<item_name>.+?)" has appeared in expanded '
                 '"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that (?P<item_type>provider) '
                 'named "(?P<item_name>.+?)" has appeared in expanded '
                 '"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@when(parsers.re('user of (?P<browser_id>.+?) sees that there is '
                 '(?P<item_type>space) named "(?P<item_name>.+?)" in expanded '
                 '"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that there is '
                 '(?P<item_type>space) named "(?P<item_name>.+?)" in expanded '
                 '"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@when(parsers.re('user of (?P<browser_id>.+?) sees that (?P<item_type>space) '
                 'named "(?P<item_name>.+?)" has appeared in expanded '
                 '"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that (?P<item_type>space) '
                 'named "(?P<item_name>.+?)" has appeared in expanded '
                 '"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
def assert_there_is_item_named_in_oz_panel_list(selenium, browser_id, item_type,
                                                item_name, oz_panel, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def assert_item_exist(d, item, items_list_type, panel):
        err_msg = 'no {type} named "{name}" found in {panel} ' \
                  'oz panel'.format(type=items_list_type, name=item,
                                    panel=panel)
        list_attr = '{}s'.format(items_list_type)
        assert item in getattr(oz_page(d)[panel], list_attr), err_msg

    assert_item_exist(driver, item_name, item_type, oz_panel)


@when(parsers.re('user of (?P<browser_id>.+?) sees that (?P<item_type>provider) '
                 'named "(?P<item_name>.+?)" has disappeared from expanded '
                 '"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that (?P<item_type>provider) '
                 'named "(?P<item_name>.+?)" has disappeared from expanded '
                 '"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@when(parsers.re('user of (?P<browser_id>.+?) sees that there is no '
                 '(?P<item_type>provider) named "(?P<item_name>.+?)" '
                 'in expanded "(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that there is no '
                 '(?P<item_type>provider) named "(?P<item_name>.+?)" '
                 'in expanded "(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@when(parsers.re('user of (?P<browser_id>.+?) sees that (?P<item_type>space) '
                 'named "(?P<item_name>.+?)" has disappeared from expanded '
                 '"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that (?P<item_type>space) '
                 'named "(?P<item_name>.+?)" has disappeared from expanded '
                 '"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@when(parsers.re('user of (?P<browser_id>.+?) sees that there is no '
                 '(?P<item_type>space) named "(?P<item_name>.+?)" in expanded '
                 '"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@when(parsers.re('user of (?P<browser_id>.+?) sees that there is no '
                 '(?P<item_type>space) named "(?P<item_name>.+?)" in expanded '
                 '"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
def assert_there_is_no_item_named_in_oz_panel_list(selenium, browser_id,
                                                   item_type, item_name,
                                                   oz_panel, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def assert_item_not_exist(d, item, items_list_type, panel):
        err_msg = '{type} named "{name}" found in {panel} oz panel while it ' \
                  'should not be found'.format(type=items_list_type,
                                               name=item, panel=panel)
        list_attr = '{}s'.format(items_list_type)
        assert item not in getattr(oz_page(d)[panel], list_attr), err_msg

    assert_item_not_exist(driver, item_name, item_type, oz_panel)
