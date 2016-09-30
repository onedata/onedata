"""Generic GUI testing utils - mainly helpers and extensions for Selenium.
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
import os
from tests import gui
from tests.gui.conftest import SELENIUM_IMPLICIT_WAIT, WAIT_REFRESH, WAIT_FRONTEND
from selenium.webdriver.support.wait import WebDriverWait as Wait
from selenium.common.exceptions import TimeoutException

from inspect import selector


# RE_URL regexp is matched as shown below:
#
# https://172.18.0.8/#/onedata/data/small_space/g2gDZAAEZ3VpZG0AAAAkZzJnQ
# \       \        /   \     / \  / \         /                        /
#  \        domain      \   /   tab  \___id__/                        /
#   \            /      access                                       /
#    \_base_url_/         \_________________method__________________/

RE_URL = re.compile(r'(?P<base_url>https?://(?P<domain>.*?))'
                    r'(/#)?(?P<method>/(?P<access>[^/]*)/(?P<tab>[^/]*)'
                    r'(/(?P<id>[^/]*).*)?)')


def parse_url(url):
    return RE_URL.match(url)


def go_to_relative_url(selenium, relative_url):
    new_url = RE_URL.match(selenium.current_url).group('base_url') + relative_url
    selenium.get(new_url)


# A draft - not tested yet, but can be helpful in the future
def change_implicit_wait(driver, fun, wait_time):
    """This will invoke fun(driver), chaining implicitly_wait for time of execution
    WARNING: this will change implicit_wait time on global selenium object!
    Returns the result of fun invocation
    """
    result = None
    try:
        driver.implicitly_wait(wait_time)
        result = fun(driver)
    finally:
        driver.implicitly_wait(SELENIUM_IMPLICIT_WAIT)

    return result


def upload_file_path(file_name):
    """Resolve an absolute path for file with name file_name stored in upload_files dir
    """
    return os.path.join(
        os.path.dirname(os.path.abspath(gui.__file__)),
        'upload_files',
        file_name
    )


def refresh_and_call(browser, callback, *args, **kwargs):
    """Refresh browser and keep calling callback with given args
    until achieve expected result or timeout.
    """
    browser.refresh()
    try:
        result = Wait(browser, WAIT_REFRESH).until(
            lambda s: callback(s, *args, **kwargs)
        )
    except TimeoutException:
        return None
    else:
        return result


def find_item_with_given_properties(browser, css_path, check_properties):
    """Find elements with given css selector and return first one fulfilling
    given properties.
    """
    items = browser.find_elements_by_css_selector(css_path)
    for item in items:
        if check_properties(item):
            return item
    return None


def click_on_element(browser, css_path, item_name,
                     msg, ignore_case=True,
                     wait=WAIT_FRONTEND):
    """Check if elem is visible and enabled, if so click on it.
    """
    properties = selector(browser, text=item_name,
                          ignore_case=ignore_case,
                          check_visibility=True,
                          check_if_enabled=True)

    Wait(browser, wait).until(
        lambda s: find_item_with_given_properties(s, css_path,
                                                  properties),
        message=msg.format(item_name) if item_name else msg
    ).click()


def enter_text(input_box, text):
    input_box.clear()
    input_box.send_keys(text)
    return True if input_box.get_attribute('value') == text else False
