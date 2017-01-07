"""Steps for features of Onezone login page.
"""

from tests.gui.conftest import WAIT_FRONTEND
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






@when(parsers.parse('user of {browser_id} clicks on "{btn_name}" button '
                    'in expanded "{panel_name}" Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on "{btn_name}" button '
                    'in expanded "{panel_name}" Onezone panel'))
def click_on_btn_in_oz_panel(selenium, browser_id, btn_name, panel_name):
    driver = select_browser(selenium, browser_id)
    panel = panel_to_css[panel_name.lower()]
    btn_name = btn_name.lower()

    def _get_btn(d):
        buttons = d.find_elements_by_css_selector('{} .clickable'
                                                  ''.format(panel))
        for btn in buttons:
            if btn.text.lower() == btn_name:
                return btn

    Wait(driver, WAIT_FRONTEND).until(
        lambda d: _get_btn(d),
        message='no button named "{}" found'.format(btn_name)
    ).click()


@when(parsers.parse('user of {browser_id} clicks on input box next to '
                    '{icon} icon in expanded "{panel}" Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on input box next to '
                    '{icon} icon in expanded "{panel}" Onezone panel'))
def oz_activate_input_next_to_icon(selenium, browser_id, icon, panel):
    driver = select_browser(selenium, browser_id)
    header = _get_active_heading_components(driver,
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
def oz_click_on_btn_for_in_box_next_to_icon(selenium, browser_id,
                                            btn_type, icon, panel):
    driver = select_browser(selenium, browser_id)
    header = _get_active_heading_components(driver,
                                            panel_to_css[panel.lower()],
                                            icon_to_css[icon.lower()])

    btn_css = 'check' if btn_type == 'confirm' else 'x'
    header.find_element_by_css_selector('.oneicon-checkbox-{}'
                                        ''.format(btn_css)).click()
