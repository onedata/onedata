"""Steps for USER ALIAS panel features of Onezone page.
"""

from pytest_bdd import when, then, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser

from tests.gui.conftest import WAIT_BACKEND
from tests.gui.utils.generic import repeat_failed

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@when(parsers.parse('user of {browser_id} records his current alias '
                    'displayed in "USER ALIAS" Onezone panel'))
@then(parsers.parse('user of {browser_id} records his current alias '
                    'displayed in "USER ALIAS" Onezone panel'))
@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def record_oz_usr_alias(selenium, browser_id, tmp_memory, oz_page):
    driver = select_browser(selenium, browser_id)
    usr_alias = oz_page(driver)['user alias'].alias
    tmp_memory[browser_id]['user_alias'] = usr_alias


@when(parsers.parse('user of {browser_id} activates edit box by clicking on '
                    'the user alias in expanded "USER ALIAS" Onezone panel'))
@then(parsers.parse('user of {browser_id} activates edit box by clicking on '
                    'the user alias in expanded "USER ALIAS" Onezone panel'))
@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def click_user_alias_edit(selenium, browser_id, tmp_memory, oz_page):
    driver = select_browser(selenium, browser_id)
    alias = oz_page(driver)['user alias']
    tmp_memory[browser_id]['edit_box'] = alias.edit_alias()


@when(parsers.parse('user of {browser_id} types recorded alias to user '
                    'alias edit box in "USER ALIAS" Onezone panel'))
@then(parsers.parse('user of {browser_id} types recorded alias to user '
                    'alias edit box in "USER ALIAS" Onezone panel'))
def type_usr_alias_into_usr_alias_edit_box(browser_id, tmp_memory):
    recorded_usr_alias = tmp_memory[browser_id]['user_alias']
    edit_box = tmp_memory[browser_id]['edit_box']
    edit_box.value = recorded_usr_alias


@when(parsers.parse('user of {browser_id} sees that the user alias displayed '
                    'in "USER ALIAS" Onezone panel is "{usr_alias}"'))
@then(parsers.parse('user of {browser_id} sees that the user alias displayed '
                    'in "USER ALIAS" Onezone panel is "{usr_alias}"'))
@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def user_alias_equals_to_text(selenium, browser_id, usr_alias, oz_page):
    driver = select_browser(selenium, browser_id)
    displayed_alias = oz_page(driver)['user alias'].alias
    assert displayed_alias == usr_alias, \
        'displayed "{}" as usr alias in USER ALIAS oz panel ' \
        'instead of "{}"'.format(displayed_alias, usr_alias)


@when(parsers.parse('user of {browser_id} sees that the user alias displayed '
                    'in "USER ALIAS" Onezone panel is as recorded one'))
@then(parsers.parse('user of {browser_id} sees that the user alias displayed '
                    'in "USER ALIAS" Onezone panel is as recorded one'))
@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def user_alias_equals_recorded_alias(selenium, browser_id,
                                     tmp_memory, oz_page):
    driver = select_browser(selenium, browser_id)
    recorded_alias = tmp_memory[browser_id]['user_alias']
    displayed_alias = oz_page(driver)['user alias'].alias
    assert displayed_alias == recorded_alias, \
        'displayed user alias in USER ALIAS oz panel is "{}" ' \
        'instead of "{}"'.format(displayed_alias, recorded_alias)
