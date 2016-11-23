"""Steps for features of Onezone login page.
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re

from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from tests.utils.acceptance_utils import list_parser

from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.common.keys import Keys

from pytest_bdd import given, when, then, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser


panel_to_css = {'access tokens': '#collapse-tokens',
                'go to your files': '#collapse-providers',
                'data space management': '#collapse-spaces',
                'user alias': '#collapse-alias',
                'authentication settings': '#collapse-accounts'}


icon_to_css = {'create space': 'oneicon-space-add'}


def _uncollapse_oz_panel(driver, name):
    re_lc_name = re.compile(name, re.I)

    def sidebar_group_by_name(d):
        groups = d.find_elements_by_css_selector('.main-accordion-group')
        for g in groups:
            t = g.find_element_by_css_selector('a.main-accordion-toggle')
            if re_lc_name.match(t.text):
                return g, t
        return None

    group, toggle = Wait(driver, WAIT_FRONTEND).until(
        sidebar_group_by_name,
        message='searching for {:s} toggle'.format(name)
    )

    aria_expanded = toggle.get_attribute('aria-expanded')
    if aria_expanded is None or aria_expanded == 'false':
        toggle.click()

    return group


def _get_clicked_heading_components(driver, panel, icon):
    items = driver.find_elements_by_css_selector('{panel} a.clickable, '
                                                 '{panel} a.clickable > '
                                                 '.secondary-icon > .{icon}'
                                                 ''.format(panel=panel,
                                                           icon=icon))
    header = None
    for item in items:
        if icon in item.get_attribute('class'):
            return header
        header = item


@given(parsers.re('users? of (?P<browser_id_list>.*) expanded the '
                  '"(?P<name>.*)" Onezone sidebar panel'))
def g_uncollapse_oz_panel(selenium, browser_id_list, name):
    for browser_id in list_parser(browser_id_list):
        driver = select_browser(selenium, browser_id)
        _uncollapse_oz_panel(driver, name)


@when(parsers.re('users? of (?P<browser_id_list>.*) expands? the '
                 '"(?P<name>.*)" Onezone sidebar panel'))
@then(parsers.re('users? of (?P<browser_id_list>.*) expands? the '
                 '"(?P<name>.*)" Onezone sidebar panel'))
def wt_uncollapse_oz_panel(selenium, browser_id_list, name):
    for browser_id in list_parser(browser_id_list):
        driver = select_browser(selenium, browser_id)
        _uncollapse_oz_panel(driver, name)


@when(parsers.parse('user of {browser_id} clicks on "{btn_name}" button '
                    'in expanded "{panel_name}" panel'))
@then(parsers.parse('user of {browser_id} clicks on "{btn_name}" button '
                    'in expanded "{panel_name}" panel'))
def click_on_btn_in_panel(selenium, browser_id, btn_name, panel_name):
    driver = select_browser(selenium, browser_id)
    panel = panel_to_css[panel_name.lower()]
    btn_name = btn_name.lower()

    def _get_btn(d):
        buttons = d.find_elements_by_css_selector('{} a.clickable'
                                                  ''.format(panel))
        for btn in buttons:
            if btn.text.lower() == btn_name:
                btn.click()
                return True
        else:
            return False

    Wait(driver, WAIT_FRONTEND).until(
        lambda d: _get_btn(d),
        message='no button named "{}" found'.format(btn_name)
    )


@when(parsers.parse('user of {browser_id} clicks on input box next to '
                    '{icon} icon in expanded "{panel}" Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on input box next to '
                    '{icon} icon in expanded "{panel}" Onezone panel'))
def activate_input_next_to_icon(selenium, browser_id, icon, panel):
    driver = select_browser(selenium, browser_id)
    header = _get_clicked_heading_components(driver,
                                             panel_to_css[panel.lower()],
                                             icon_to_css[icon.lower()])
    Wait(driver, WAIT_FRONTEND).until(
        lambda _: header.find_element_by_css_selector('input'),
        message='waiting for input box next to {} icon to appear'.format(icon)
    ).send_keys(Keys.NULL)


@when(parsers.re('user of (?P<browser_id>.+?) clicks on '
                 '(?P<btn_type>confirm|cancel) button for '
                 'input box next to (?P<icon>.+?) icon '
                 'in expanded "(?P<panel>.+?)" Onezone panel'))
@then(parsers.re('user of (?P<browser_id>.+?) clicks on '
                 '(?P<btn_type>confirm|cancel) button for '
                 'input box next to (?P<icon>.+?) icon '
                 'in expanded "(?P<panel>.+?)" Onezone panel'))
def click_on_btn_for_in_box_next_to_icon(selenium, browser_id, btn_type,
                                         icon, panel):
    driver = select_browser(selenium, browser_id)
    header = _get_clicked_heading_components(driver,
                                             panel_to_css[panel.lower()],
                                             icon_to_css[icon.lower()])
    header.find_element_by_css_selector('.oneicon-checkbox-{}'
                                        ''.format('check'
                                                  if btn_type == 'confirm'
                                                  else 'x')).click()








def _click_on_provider(driver, browser_id, name, tmp_memory):
    if browser_id in tmp_memory:
        tmp_memory[browser_id]['supporting_provider'] = name
    else:
        tmp_memory[browser_id] = {'supporting_provider': name}

    collapse_providers = driver.find_element_by_css_selector('#collapse-providers')

    Wait(driver, WAIT_FRONTEND).until(
        lambda s: collapse_providers.get_attribute('aria-expanded') == 'true',
        message='waiting for list of providers to appear'
    )

    def the_provider_is_present(s):
        providers = s.find_elements_by_css_selector('.provider-header')
        named_providers = [e for e in providers if e.text == name]
        if len(named_providers) > 0:
            return named_providers[0]
        else:
            return None

    Wait(driver, WAIT_FRONTEND).until(
        the_provider_is_present,
        message='waiting for provider {:s} to appear on the list'.format(name)
    ).click()


@given(parsers.re('users? of (?P<browser_id_list>.*) clicked on the "(?P<name>.*)" '
                  'provider in Onezone providers sidebar panel'))
def g_click_on_provider_in_sidebar(selenium, browser_id_list, name, tmp_memory):
    for browser_id in list_parser(browser_id_list):
        driver = select_browser(selenium, browser_id)
        _click_on_provider(driver, browser_id, name, tmp_memory)
