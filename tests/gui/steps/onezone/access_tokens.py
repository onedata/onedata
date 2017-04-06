"""Steps for access tokens features in Onezone login page.
"""

import pyperclip
import time
from pytest_bdd import given

from tests.gui.conftest import WAIT_BACKEND, SELENIUM_IMPLICIT_WAIT
from tests.gui.utils.generic import repeat_failed, implicit_wait

from pytest_bdd import when, then, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


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
    driver = select_browser(selenium, browser_id)
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
    driver = select_browser(selenium, browser_id)
    _click_on_btn_for_token(driver, oz_page, ordinal, btn)


@given(parsers.parse(r'user of {browser_id} recorded copied access token '
                     r'for future use'))
@repeat_failed(timeout=WAIT_BACKEND)
def g_record_copied_access_token(browser_id, tmp_memory):
    tmp_memory[browser_id]['access_token'] = pyperclip.paste()


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
                                                     ordinal, oz_page):
    driver = select_browser(selenium, browser_id)
    val = oz_page(driver)['access tokens'].tokens[int(ordinal[:-2]) - 1].value
    copied_val = pyperclip.paste()
    assert val == copied_val, 'Access Token has been copied incorrectly. ' \
                              'Expected {}, got {}'.format(val, copied_val)


@when(parsers.parse('user of {browser_id} sees exactly {num:d} item(s) '
                    'on tokens list in expanded "ACCESS TOKENS" Onezone panel'))
@then(parsers.parse('user of {browser_id} sees exactly {num:d} item(s) '
                    'on tokens list in expanded "ACCESS TOKENS" Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def assert_oz_access_tokens_list_has_num_tokens(selenium, browser_id,
                                                num, oz_page):
    driver = select_browser(selenium, browser_id)
    with implicit_wait(driver, 0.1, SELENIUM_IMPLICIT_WAIT):
        displayed = oz_page(driver)['access tokens'].tokens.count()
        assert displayed == num, \
            'Displayed tokens in ACCESS TOKENS oz panel: {seen} ' \
            'instead of excepted: {excepted}'.format(seen=displayed,
                                                     excepted=num)


@given(parsers.parse('user of {browser_id} created and recorded access token '
                     'for later use with CDMI API'))
@repeat_failed(timeout=WAIT_BACKEND)
def create_and_record_access_token_for_cdmi(selenium, browser_id,
                                            oz_page, tmp_memory):
    driver = select_browser(selenium, browser_id)
    panel = oz_page(driver)['access tokens']
    panel.expand()

    with implicit_wait(driver, 0.05, SELENIUM_IMPLICIT_WAIT):
        if panel.tokens.count() > 0:
            panel.tokens[0].copy()
        else:
            panel.create_new_access_token()
            now = time.time()
            time_limit = now + 10
            while now < time_limit:
                try:
                    panel.tokens[0].copy()
                except (RuntimeError, Exception) as ex:
                    print ex
                    now = time.time()
                else:
                    break
            else:
                raise RuntimeError("couldn't create and copy "
                                   "access token in oz")

    tmp_memory[browser_id]['access_token'] = pyperclip.paste()
