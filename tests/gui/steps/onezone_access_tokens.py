"""Steps for access tokens features in Onezone login page.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import pyperclip

from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.steps.onezone_logged_in_common import panel_to_css

from selenium.webdriver.support.ui import WebDriverWait as Wait

from pytest_bdd import when, then, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser


@when(parsers.re(r'user of (?P<browser_id>.*?) clicks on (?P<icon_type>.*?) '
                 r'icon for (?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                 r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                 r'item on tokens list in expanded "ACCESS TOKENS" panel'))
@then(parsers.re(r'user of (?P<browser_id>.*?) clicks on (?P<icon_type>.*?) '
                 r'icon for (?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                 r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                 r'item on tokens list in expanded "ACCESS TOKENS" panel'))
def click_on_btn_for_selected_token(selenium, browser_id, icon_type, ordinal):
    driver = select_browser(selenium, browser_id)
    panel = panel_to_css['access tokens']
    tokens = driver.find_elements_by_css_selector('{} .tokens-list-item'
                                                  ''.format(panel))
    token = tokens[int(ordinal[:-2])-1]
    btn = token.find_element_by_css_selector('.oneicon-{}'.format(icon_type))
    Wait(driver, WAIT_FRONTEND).until(
        lambda _: btn.is_displayed(),
        message='waiting for {} btn for {} token to appear'.format(icon_type,
                                                                   ordinal)
    ).click()


@when(parsers.re(r'user of (?P<browser_id>.+?) sees that token for '
                 r'(?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                 r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                 r'item on tokens list in expanded "ACCESS TOKENS" panel '
                 r'has been copied correctly'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that token for '
                 r'(?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                 r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                 r'item on tokens list in expanded "ACCESS TOKENS" panel '
                 r'has been copied correctly'))
def has_token_been_copied_correctly(selenium, browser_id, ordinal):
    driver = select_browser(selenium, browser_id)
    panel = panel_to_css['access tokens']
    tokens = driver.find_elements_by_css_selector('{} .tokens-list-item input'
                                                  ''.format(panel))
    token_val = tokens[int(ordinal[:-2])-1].get_attribute('value')
    copied_val = pyperclip.paste()
    assert token_val == copied_val


@when(parsers.parse('user of {browser_id} sees exactly {num:d} item(s)'
                    ' on tokens list in expanded "ACCESS TOKENS" panel'))
@then(parsers.parse('user of {browser_id} sees exactly {num:d} item(s)'
                    ' on tokens list in expanded "ACCESS TOKENS" panel'))
def has_token_list_enough_items(selenium, browser_id, num):
    driver = select_browser(selenium, browser_id)
    panel = panel_to_css['access tokens']
    tokens = driver.find_elements_by_css_selector('{} .tokens-list-item'
                                                  ''.format(panel))
    err_msg = 'there is {} tokens displayed ' \
              'instead of expected {}'.format(len(tokens), num)
    assert len(tokens) == num, err_msg
