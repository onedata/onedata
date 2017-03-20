"""Steps for features of Oneprovider's spaces.
"""


from selenium.common.exceptions import NoSuchElementException

from pytest_bdd import parsers, given, when, then
from pytest_selenium_multi.pytest_selenium_multi import select_browser

from tests.gui.utils.generic import implicit_wait, repeat_failed
from tests.gui.conftest import SELENIUM_IMPLICIT_WAIT, WAIT_BACKEND

__author__ = "Michal Cwiertnia, Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def _is_home_space(driver, space_name):
    spaces = driver.find_elements_by_css_selector('ul.spaces-list '
                                                  'li:not([class~="clickable"])')
    displayed_name = ''
    with implicit_wait(driver, 0.01, SELENIUM_IMPLICIT_WAIT):
        for space in spaces:
            try:
                space.find_element_by_css_selector('.oneicon-space-home')
            except NoSuchElementException:
                continue
            else:
                displayed_name = space.find_element_by_css_selector('.item-'
                                                                    'label')
    err_msg = 'home space is {} instead of {}'.format(displayed_name.text,
                                                      space_name)
    assert displayed_name.text == space_name, err_msg


@given(parsers.parse('user of {browser_id} seen that home space icon '
                     'was displayed next to name of space "{space_name}" '
                     'in spaces list'))
def g_assert_home_space_is_space_named(selenium, browser_id, space_name):
    driver = select_browser(selenium, browser_id)
    _is_home_space(driver, space_name)


@when(parsers.parse('user of {browser_id} sees that home space icon '
                    'is displayed next to name of space '
                    '"{space_name}" in spaces list'))
@then(parsers.parse('user of {browser_id} sees that home space icon '
                    'is displayed next to name of space '
                    '"{space_name}" in spaces list'))
@when(parsers.parse('user of {browser_id} sees that home space icon '
                    'has appeared next to displayed '
                    'name of space "{space_name}" in spaces list'))
@then(parsers.parse('user of {browser_id} sees that home space icon '
                    'has appeared next to displayed '
                    'name of space "{space_name}" in spaces list'))
def wt_assert_home_space_is_space_named(selenium, browser_id, space_name):
    driver = select_browser(selenium, browser_id)
    _is_home_space(driver, space_name)
