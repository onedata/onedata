"""Steps for user alias features of Onezone login page.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from tests.gui.steps.onezone_logged_in_common import panel_to_css

from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.common.keys import Keys
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.support.expected_conditions import staleness_of

from pytest_bdd import when, then, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser


def _wait_for_loader_to_disappear(driver, css_selector):
    try:
        loader = driver.find_element_by_css_selector(css_selector)
    except NoSuchElementException:
        return
    else:
        Wait(driver, WAIT_BACKEND).until(
            staleness_of(loader),
            message='waiting for loader of user alias to disappear'
        )


@when(parsers.parse('user of {browser_id} clicks on the user alias '
                    'in expanded "USER ALIAS" panel'))
@then(parsers.parse('user of {browser_id} clicks on the user alias '
                    'in expanded "USER ALIAS" panel'))
def click_user_alias_edit(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    panel = panel_to_css['user alias']
    _wait_for_loader_to_disappear(driver, '{} .spinner-container'.format(panel))
    alias_header = driver.find_element_by_css_selector('{} a .space-header'
                                                       ''.format(panel))
    Wait(driver, WAIT_FRONTEND).until(
        lambda _: alias_header.is_displayed(),
        message='waiting for user alias to become visible'
    )
    alias_header.click()
    # .spinner-container
    # activate input box
    Wait(driver, WAIT_FRONTEND).until(
        lambda _: alias_header.find_element_by_css_selector('input'),
        message='waiting for user alias input box to become active'
    ).send_keys(Keys.NULL)


@when(parsers.parse('user of {browser_id} sees that '
                    'the user alias is "{user_name}"'))
@then(parsers.parse('user of {browser_id} sees that '
                    'the user alias is "{user_name}"'))
def user_alias_equals_to(selenium, browser_id, user_name):
    driver = select_browser(selenium, browser_id)
    panel = panel_to_css['user alias']
    alias_header = driver.find_element_by_css_selector('{} a .space-header'
                                                       ''.format(panel))
    Wait(driver, WAIT_BACKEND).until(
        lambda s: alias_header.text == user_name,
        message='waiting for user alias to become {:s}'.format(user_name)
    )


@when(parsers.parse('user of {browser_id} sees that '
                    'the user alias is as recorded one'))
@then(parsers.parse('user of {browser_id} sees that '
                    'the user alias is as recorded one'))
def user_alias_equals_prev_alias(selenium, browser_id, tmp_memory):
    driver = select_browser(selenium, browser_id)
    panel = panel_to_css['user alias']
    user_alias = tmp_memory[browser_id]['user_alias']
    alias_header = driver.find_element_by_css_selector('{} a .space-header'
                                                       ''.format(panel))
    Wait(driver, WAIT_BACKEND).until(
        lambda s: alias_header.text == user_alias,
        message='waiting for user alias to become {:s}'.format(user_alias)
    )


@when(parsers.parse('user of {browser_id} records his '
                    'current alias'))
@then(parsers.parse('user of {browser_id} records his '
                    'current alias'))
def record_user_alias(selenium, browser_id, tmp_memory):
    driver = select_browser(selenium, browser_id)
    panel = panel_to_css['user alias']
    alias_header = driver.find_element_by_css_selector('{} a .space-header'
                                                       ''.format(panel))

    tmp_memory[browser_id]['user_alias'] = Wait(driver, WAIT_BACKEND).until(
        lambda s: alias_header.text,
        message='waiting for user alias to become visible'
    )


@when(parsers.re('user of (?P<browser_id>.+?) clicks on '
                 '(?P<btn_type>confirm|cancel) button '
                 'displayed next to user alias edit box'))
@when(parsers.re('user of (?P<browser_id>.+?) clicks on '
                 '(?P<btn_type>confirm|cancel) button '
                 'displayed next to user alias edit box'))
def ua_click_on_btn_next_to_edit_box(selenium, browser_id, btn_type):
    driver = select_browser(selenium, browser_id)
    panel = panel_to_css['user alias']
    btn = driver.find_element_by_css_selector('{} a .oneicon-checkbox-{}'
                                              ''.format(panel,
                                                        'check'
                                                        if btn_type == 'confirm'
                                                        else 'x'))
    btn.click()
