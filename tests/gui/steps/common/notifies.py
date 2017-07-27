"""This module contains gherkin steps to run acceptance tests featuring
ember notifies in web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


import re

from pytest_bdd import when, then, parsers

from selenium.common.exceptions import (NoSuchElementException,
                                        StaleElementReferenceException)
from selenium.webdriver.support.expected_conditions import staleness_of

from tests.gui.utils.generic import suppress, repeat_failed
from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND


@when(parsers.parse('user of {browser_id} sees an {notify_type} notify '
                    'with text matching to: {text_regexp}'))
@then(parsers.parse('user of {browser_id} sees an {notify_type} notify '
                    'with text matching to: {text_regexp}'))
@repeat_failed(timeout=2 * WAIT_BACKEND)
def notify_visible_with_text(selenium, browser_id, notify_type, text_regexp):
    driver = selenium[browser_id]
    css_sel = '.ember-notify-show[class*={}] .message'.format(notify_type)
    regexp = re.compile(text_regexp)
    with suppress(NoSuchElementException, StaleElementReferenceException):
        assert any(regexp.match(notify.text) for notify
                   in driver.find_elements_by_css_selector(css_sel)), \
            'no {} notify with "{}" msg found'.format(notify_type,
                                                      text_regexp)


@when(parsers.parse('user of {browser_id} closes all notifies'))
@then(parsers.parse('user of {browser_id} closes all notifies'))
@repeat_failed(timeout=WAIT_FRONTEND)
def close_visible_notifies(selenium, browser_id):
    driver = selenium[browser_id]
    notifies = driver.find_elements_by_css_selector('.ember-notify '
                                                    'a.close-button')

    with suppress(StaleElementReferenceException):
        map(lambda btn: btn.click(), notifies)

    assert all(staleness_of(notify) for notify in notifies), \
        'not all notifies were closed'
