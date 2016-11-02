"""Helper functions inspecting if given web element fulfil some criterion
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re

from selenium.common.exceptions import TimeoutException
from tests.gui.conftest import WAIT_FRONTEND
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.wait import WebDriverWait as Wait


def is_active(browser, web_element):
    """Check if given web element is the active one in given browser.
    """
    try:
        Wait(browser, WAIT_FRONTEND).until(
            lambda s: web_element == s.switch_to.active_element
        )
    except TimeoutException:
        return False
    else:
        return True


def is_visible(browser, web_element):
    """Check if given web element is visible in given browser.
    """
    try:
        Wait(browser, WAIT_FRONTEND).until(EC.visibility_of(web_element))
    except TimeoutException:
        return False
    else:
        return True


def is_enabled(web_element):
    """Check if given web element is enabled in given browser.
    """
    return web_element.is_enabled()


def contains_text_of_given_pattern(web_element, pattern):
    """Check if given web element contains given pattern.
    """
    return re.match(pattern, web_element.text)


def selector(browser, text='', ignore_case=False, check_visibility=False,
             check_if_enabled=False, check_if_active=False):
    """Chain criterion checking and return function that will conduct check.
    """
    conditions = []
    if text:
        text_regexp = re.compile(text, re.I if ignore_case else 0)
        conditions.append(lambda item:
                          contains_text_of_given_pattern(item, text_regexp)
                          )
    if check_visibility:
        conditions.append(lambda item: is_visible(browser, item))
    if check_if_enabled:
        conditions.append(is_enabled)
    if check_if_active:
        conditions.append(lambda item: is_active(browser, item))

    def _select(item):
        return all(condition(item) for condition in conditions)

    return _select
