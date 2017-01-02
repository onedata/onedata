"""Steps for features of Onezone space panel page.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


import pyperclip

from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from tests.gui.steps.onezone_panel_list import get_submenu_for_item_from_oz_panel_list
from tests.utils.acceptance_utils import list_parser

from selenium.webdriver.support.ui import WebDriverWait as Wait

from pytest_bdd import when, then, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser


def get_supporting_providers(submenu):
    css_sel = 'ul li:not([class=get-support]), ' \
              'ul li:not([class=get-support]) .icon .one-icon, ' \
              'ul li:not([class=get-support]) .label'

    items = submenu.find_elements_by_css_selector(css_sel)
    providers = {}
    for row, icon, label in zip(items[::3], items[1::3], items[2::3]):
        if 'oneicon-provider' in icon.get_attribute('class'):
            providers[label.text] = row

    return providers


@when(parsers.parse('user of {browser_id} sees that submenu for '
                    'space named "{item_name}" in spaces list '
                    'in expanded "{panel}" Onezone panel contains '
                    '{providers_list} supporting provider(s) '
                    'and Get support button'))
@then(parsers.parse('user of {browser_id} sees that submenu for '
                    'space named "{item_name}" in spaces list '
                    'in expanded "{panel}" Onezone panel contains '
                    '{providers_list} supporting provider(s) '
                    'and Get support button'))
def oz_check_submenu_content_for_space(selenium, browser_id, item_name,
                                       panel, providers_list):
    driver = select_browser(selenium, browser_id)
    item_type = 'space'

    submenu = Wait(driver, WAIT_BACKEND).until(
        lambda d: get_submenu_for_item_from_oz_panel_list(d, item_name,
                                                          item_type, panel),
        message='waiting for submenu for "{}" in {} list to appear'
                ''.format(item_name, item_type)
    )

    supporting_providers = get_supporting_providers(submenu)
    for provider in list_parser(providers_list):
        assert provider in supporting_providers, \
            '{} is not listed as supporting provider'.format(provider)

    assert Wait(driver, WAIT_FRONTEND).until(
        lambda d: submenu.find_element_by_css_selector('ul li.get-support'),
        message='submenu for "{}" in {} list has not been expanded'
                ''.format(item_name, item_name)
    )


@when(parsers.parse('user of {browser_id} clicks on Get support for '
                    'space named "{item_name}" in spaces list '
                    'in expanded "{panel}" Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on Get support for '
                    'space named "{item_name}" in spaces list '
                    'in expanded "{panel}" Onezone panel'))
def oz_click_on_get_support_btn_for_space(selenium, browser_id,
                                          item_name, panel):
    driver = select_browser(selenium, browser_id)
    item_type = 'space'

    submenu = Wait(driver, WAIT_BACKEND).until(
        lambda d: get_submenu_for_item_from_oz_panel_list(d, item_name,
                                                          item_type, panel),
        message='waiting for submenu for "{}" in {} list to appear'
                ''.format(item_name, item_type)
    )

    submenu.find_element_by_css_selector('ul li.get-support '
                                         'a.clickable').click()


def _get_token_popup_displayed(submenu):
    dropdown = submenu.find_elements_by_css_selector('ul li.get-support '
                                                     'a.clickable, '
                                                     'ul li.get-support '
                                                     'a.clickable + '
                                                     '.token-popup')
    btn, popup = dropdown
    if 'true' in btn.get_attribute('aria-expanded') and popup.is_displayed():
        return popup
    else:
        return None


@when(parsers.parse('user of {browser_id} sees that token popup for '
                    'space named "{item_name}" in spaces list '
                    'in expanded "{panel}" Onezone panel has appeared'))
@then(parsers.parse('user of {browser_id} sees that token popup for '
                    'space named "{item_name}" in spaces list '
                    'in expanded "{panel}" Onezone panel has appeared'))
def oz_has_token_popup_appeared(selenium, browser_id, item_name, panel):
    driver = select_browser(selenium, browser_id)
    item_type = 'space'

    submenu = Wait(driver, WAIT_BACKEND).until(
        lambda d: get_submenu_for_item_from_oz_panel_list(d, item_name,
                                                          item_type, panel),
        message='waiting for submenu for "{}" in {} list to appear'
                ''.format(item_name, item_type)
    )

    Wait(driver, WAIT_BACKEND).until(
        lambda d: _get_token_popup_displayed(submenu),
        message='waiting for token popup for {} to appear'.format(item_name)
    )


@when(parsers.parse('user of {browser_id} can copy visible token from '
                    'popup for space named "{item_name}" in spaces list '
                    'in expanded "{panel}" Onezone panel has appeared'))
@then(parsers.parse('user of {browser_id} can copy visible token from '
                    'popup for space named "{item_name}" in spaces list '
                    'in expanded "{panel}" Onezone panel has appeared'))
def oz_has_token_popup_appeared(selenium, browser_id, item_name, panel):
    driver = select_browser(selenium, browser_id)
    item_type = 'space'

    submenu = Wait(driver, WAIT_BACKEND).until(
        lambda d: get_submenu_for_item_from_oz_panel_list(d, item_name,
                                                          item_type, panel),
        message='waiting for submenu for "{}" in {} list to appear'
                ''.format(item_name, item_type)
    )

    popup = Wait(driver, WAIT_BACKEND).until(
        lambda d: _get_token_popup_displayed(submenu),
        message='waiting for token popup for {} to appear'.format(item_name)
    )

    popup.find_element_by_css_selector('button').click()
    copied_token = pyperclip.paste()
    token = popup.find_element_by_css_selector('input').get_attribute('value')
    assert copied_token == token, 'copied token {} is not equal to generated token {}'.format(copied_token, token)
