"""This module contains gherkin steps to run acceptance tests featuring
access tokens management in onezone web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


from pytest_bdd import given, when, then, parsers

from tests.gui.conftest import WAIT_BACKEND
from tests.gui.utils.generic import repeat_failed


def _click_on_btn_for_token(driver, oz_page, ordinal, btn):
    token = oz_page(driver)['access tokens'].tokens[int(ordinal[:-2]) - 1]
    action = getattr(token, btn)
    action()


@given(parsers.re(r'user of (?P<browser_id>.*?) clicked on (?P<btn>copy|remove) '
                  r'icon for (?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                  r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                  r'item on tokens list in expanded "ACCESS TOKENS" Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def g_click_on_btn_for_oz_access_token(selenium, browser_id, btn,
                                       ordinal, oz_page):
    driver = selenium[browser_id]
    _click_on_btn_for_token(driver, oz_page, ordinal, btn)


@when(parsers.re(r'user of (?P<browser_id>.*?) clicks on (?P<btn>copy|remove) '
                 r'icon for (?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                 r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                 r'item on tokens list in expanded "ACCESS TOKENS" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.*?) clicks on (?P<btn>copy|remove) '
                 r'icon for (?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                 r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                 r'item on tokens list in expanded "ACCESS TOKENS" Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def wt_click_on_btn_for_oz_access_token(selenium, browser_id, btn,
                                        ordinal, oz_page):
    driver = selenium[browser_id]
    _click_on_btn_for_token(driver, oz_page, ordinal, btn)


@given(parsers.parse(r'user of {browser_id} recorded copied access token '
                     r'for future use'))
@repeat_failed(timeout=WAIT_BACKEND)
def g_record_copied_access_token(browser_id, tmp_memory, displays, clipboard):
    token = clipboard.paste(display=displays[browser_id])
    tmp_memory[browser_id]['access_token'] = token


@when(parsers.re(r'user of (?P<browser_id>.+?) sees that token for '
                 r'(?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                 r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                 r'item on tokens list in expanded "ACCESS TOKENS" Onezone '
                 r'panel has been copied correctly'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that token for '
                 r'(?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                 r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                 r'item on tokens list in expanded "ACCESS TOKENS" Onezone '
                 r'panel has been copied correctly'))
def assert_oz_access_token_has_been_copied_correctly(selenium, browser_id,
                                                     ordinal, oz_page,
                                                     displays, clipboard):
    driver = selenium[browser_id]
    val = oz_page(driver)['access tokens'].tokens[int(ordinal[:-2]) - 1].value
    copied_val = clipboard.paste(display=displays[browser_id])
    assert val == copied_val, 'Access Token has been copied incorrectly. ' \
                              'Expected {}, got {}'.format(val, copied_val)


@when(parsers.parse('user of {browser_id} sees exactly {expected_num:d} item(s) '
                    'on tokens list in expanded "ACCESS TOKENS" Onezone panel'))
@then(parsers.parse('user of {browser_id} sees exactly {expected_num:d} item(s) '
                    'on tokens list in expanded "ACCESS TOKENS" Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def assert_oz_access_tokens_list_has_num_tokens(selenium, browser_id,
                                                expected_num, oz_page):
    driver = selenium[browser_id]
    displayed_tokens_num = oz_page(driver)['access tokens'].tokens.count()
    assert displayed_tokens_num == expected_num, \
        ('Displayed number of tokens in ACCESS TOKENS oz panel: {seen} '
         'instead of excepted: {excepted}'.format(seen=displayed_tokens_num,
                                                  excepted=expected_num))


@when(parsers.parse('user of {browser_id} clicks on "Create new '
                    'access token" button in expanded "ACCESS TOKENS" '
                    'Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on "Create new '
                    'access token" button in expanded "ACCESS TOKENS" '
                    'Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def click_on_create_new_token_in_oz_access_tokens_panel(selenium, browser_id,
                                                        oz_page):
    oz_page(selenium[browser_id])['access tokens'].create_new_access_token()
