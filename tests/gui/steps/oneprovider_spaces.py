"""Steps for features of Oneprovider's spaces.
"""

__author__ = "Michal Cwiertnia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from selenium.common.exceptions import NoSuchElementException

from pytest_bdd import parsers, then
from pytest_selenium_multi.pytest_selenium_multi import select_browser

from tests.gui.conftest import SELENIUM_IMPLICIT_WAIT
from tests.gui.utils.generic import implicit_wait


@then(parsers.parse('user of {browser_id} sees that home space icon '
                    'has appeared next to displayed '
                    'name of space "{space_name}" in spaces list'))
def check_if_home_space_icon_next_to_spaces(selenium, browser_id, space_name):
    driver = select_browser(selenium, browser_id)
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
