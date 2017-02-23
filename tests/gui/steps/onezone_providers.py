"""Steps for features of Onezone login page.
"""

from itertools import izip_longest

from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.generic import parse_seq

from selenium.webdriver.support.ui import WebDriverWait as Wait

from pytest_bdd import given, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser


__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


def _click_on_provider(driver, name):
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


@given(parsers.re('users? of (?P<browser_id_list>.*) clicked on the (?P<providers>.*) '
                  'provider in Onezone providers sidebar panel'))
def g_click_on_provider_in_sidebar(selenium, browser_id_list, providers):
    browser_ids = parse_seq(browser_id_list)
    providers = parse_seq(providers)
    for browser_id, provider in izip_longest(browser_ids, providers,
                                             fillvalue=providers[-1]):
        driver = select_browser(selenium, browser_id)
        _click_on_provider(driver, provider)
