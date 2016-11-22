"""Steps for features of provider popup in Onezone.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.gui.conftest import WAIT_FRONTEND
from tests.utils.acceptance_utils import list_parser

from selenium.webdriver.support.ui import WebDriverWait as Wait

from pytest_bdd import given, when, then, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser


def _click_on_button_in_provider_popup(driver, name):
    def go_to_files_button(s):
        links = s.find_elements_by_css_selector('.provider-place-drop a, '
                                                '.provider-place-drop button')
        for e in links:
            if e.text == name:
                return e

    Wait(driver, WAIT_FRONTEND).until(
        go_to_files_button,
        message='clicking on "{:s}" button in providers popup'.format(name)
    ).click()


@given(parsers.re('users? of (?P<browser_id_list>.*) clicked on the '
                  '"(?P<btn_name>.+?)" button in provider popup'))
def g_click_on_go_to_files_provider(selenium, browser_id_list, btn_name):
    for browser_id in list_parser(browser_id_list):
        driver = select_browser(selenium, browser_id)
        _click_on_button_in_provider_popup(driver, btn_name)


@when(parsers.re('users? of (?P<browser_id_list>.*) clicks? on the '
                 '"(?P<btn_name>.+?)" button in provider popup'))
@then(parsers.re('users? of (?P<browser_id_list>.*) clicks? on the '
                 '"(?P<btn_name>.+?)" button in provider popup'))
def wt_click_on_go_to_files_provider(selenium, browser_id_list, btn_name):
    for browser_id in list_parser(browser_id_list):
        driver = select_browser(selenium, browser_id)
        _click_on_button_in_provider_popup(driver, btn_name)
