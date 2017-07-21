"""Common steps for Oneprovider.
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.utils.generic import parse_seq
from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND

from selenium.webdriver.support.ui import WebDriverWait as Wait

from pytest_bdd import given, parsers, when, then


MAIN_MENU_TAB_TO_URL = {'spaces': 'spaces',
                        'groups': 'groups',
                        'data': 'data',
                        'shared': 'shares',
                        'providers': 'providers'}


def _click_on_tab_in_main_menu_sidebar(driver, tab):
    def _load_main_menu_tab_page():
        def _check_url(url):
            return url != driver.current_url

        current_url = driver.current_url
        driver.find_element_by_css_selector(css_path).click()

        return Wait(driver, WAIT_FRONTEND).until(
            lambda _: _check_url(current_url),
            message='waiting for url to change.'
                    'Current url: {:s}'.format(driver.current_url)
        )

    menu_tab = MAIN_MENU_TAB_TO_URL[tab]
    css_path = '.primary-sidebar a#main-{:s}'.format(menu_tab)

    Wait(driver, WAIT_BACKEND).until(
        lambda _: _load_main_menu_tab_page(),
        message='waiting for {:s} main menu tab page to load'
                ''.format(tab)
    )


@given(parsers.re('users? of (?P<browser_id_list>.*) clicked on the '
                  '"(?P<main_menu_tab>.*)" tab in main menu sidebar'))
def g_click_on_the_given_main_menu_tab(selenium, browser_id_list,
                                       main_menu_tab):
    for browser_id in parse_seq(browser_id_list):
        driver = selenium[browser_id]
        _click_on_tab_in_main_menu_sidebar(driver, main_menu_tab)


@when(parsers.re('users? of (?P<browser_id_list>.*) clicks on the '
                 '"(?P<main_menu_tab>.*)" tab in main menu sidebar'))
@then(parsers.re('users? of (?P<browser_id_list>.*) clicks on the '
                 '"(?P<main_menu_tab>.*)" tab in main menu sidebar'))
def wt_click_on_the_given_main_menu_tab(selenium, browser_id_list,
                                        main_menu_tab):
    for browser_id in parse_seq(browser_id_list):
        driver = selenium[browser_id]
        _click_on_tab_in_main_menu_sidebar(driver, main_menu_tab)


def _has_dir_content_been_loaded(driver):
    # find_element_* throws exception if nothing found
    loader = driver.find_elements_by_css_selector('#main-content '
                                                  '.loader-area-'
                                                  'content-with-'
                                                  'secondary-top')
    if loader:
        loader = loader[0]
        Wait(driver, WAIT_BACKEND).until_not(
            lambda _: loader.is_displayed(),
            message='waiting for dir content to end loading'
        )


@given(parsers.parse('user of {browser_id} sees that content of current '
                     'directory has been loaded'))
def g_has_dir_content_been_loaded(selenium, browser_id):
    driver = selenium[browser_id]
    _has_dir_content_been_loaded(driver)


@when(parsers.parse('user of {browser_id} sees that content of current '
                    'directory has been loaded'))
@then(parsers.parse('user of {browser_id} sees that content of current '
                    'directory has been loaded'))
def wt_has_dir_content_been_loaded(selenium, browser_id):
    driver = selenium[browser_id]
    _has_dir_content_been_loaded(driver)
