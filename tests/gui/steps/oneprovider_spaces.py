"""Steps for features of Oneprovider's spaces.
"""

__author__ = "Michal Cwiertnia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from pytest_bdd import parsers, given, when, then
from pytest_selenium_multi.pytest_selenium_multi import select_browser


def _is_home_space(driver, space_name):
    def _find_home_space_icon(s):
        spaces = s.find_elements_by_css_selector('ul.spaces-list '
                                                 'li:not([class~="clickable"])')
        for space in spaces:
            if space.find_element_by_css_selector('.oneicon-space-home'):
                return space
        return None

    assert _find_home_space_icon(driver).text == space_name


@given(parsers.parse('user of {browser_id} seen that home space icon '
                     'was displayed next to name of space "{space_name}" '
                     'in spaces list'))
def wt_is_home_space(selenium, browser_id, space_name):
    driver = select_browser(selenium, browser_id)
    _is_home_space(driver, space_name)


@when(parsers.parse('user of {browser_id} sees that home space icon '
                    'is displayed next to name of space "{space_name}" '
                    'in spaces list'))
@then(parsers.parse('user of {browser_id} sees that home space icon '
                    'is displayed next to name of space "{space_name}" '
                    'in spaces list'))
def wt_is_home_space(selenium, browser_id, space_name):
    driver = select_browser(selenium, browser_id)
    _is_home_space(driver, space_name)
