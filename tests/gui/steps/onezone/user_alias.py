"""Steps for USER ALIAS panel features of Onezone page.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from pytest_bdd import when, then, parsers

from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed


@when(parsers.re(r'user of (?P<browser_id>.+?) clicks on '
                 r'(?P<btn>confirm|cancel) button displayed next to user alias '
                 r'edit box in expanded "USER ALIAS" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) clicks on '
                 r'(?P<btn>confirm|cancel) button displayed next to user alias '
                 r'edit box in expanded "USER ALIAS" Onezone panel'))
def click_on_btn_for_user_alias_edit_box_in_oz(selenium, browser_id,
                                               btn, oz_page):
    getattr(oz_page(selenium[browser_id])['user alias'].edit_box, btn).click()


@when(parsers.parse('user of {browser_id} types "{text}" to user alias '
                    'edit box in expanded "USER ALIAS" Onezone panel'))
@then(parsers.parse('user of {browser_id} types "{text}" to user alias '
                    'edit box in expanded "USER ALIAS" Onezone panel'))
def type_text_into_user_alias_edit_box_in_oz(selenium, browser_id,
                                             text, oz_page):
    oz_page(selenium[browser_id])['user alias'].edit_box.value = text


@when(parsers.parse('user of {browser_id} activates edit box by clicking on '
                    'the user alias in expanded "USER ALIAS" Onezone panel'))
@then(parsers.parse('user of {browser_id} activates edit box by clicking on '
                    'the user alias in expanded "USER ALIAS" Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def activate_user_alias_edit_box_in_oz(selenium, browser_id, oz_page):
    oz_page(selenium[browser_id])['user alias'].rename()


@when(parsers.parse('user of {browser_id} sees that the user alias displayed '
                    'in "USER ALIAS" Onezone panel is "{expected_alias}"'))
@then(parsers.parse('user of {browser_id} sees that the user alias displayed '
                    'in "USER ALIAS" Onezone panel is "{expected_alias}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_correct_usr_alias_in_oz(selenium, browser_id,
                                   expected_alias, oz_page):
    displayed_alias = oz_page(selenium[browser_id])['user alias'].alias
    assert displayed_alias == expected_alias, \
        ('expected "{}" as user alias, but instead display is "{}" '
         'in USER ALIAS oz panel'.format(expected_alias, displayed_alias))
