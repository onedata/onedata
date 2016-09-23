"""Steps for features of Oneprovider shares.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


import re

from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND, MAX_REFRESH_COUNT, WAIT_REFRESH
from pytest_bdd import when, then, parsers
from selenium.webdriver.support.ui import WebDriverWait as Wait
from pytest_selenium_multi.pytest_selenium_multi import select_browser
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support.expected_conditions import staleness_of

import tests.gui.utils.file_system as fs


@when(parsers.parse('user of {browser_id} sees that new share named '
                    '"{share_name}" was created from '
                    '{item_type} "{item_name}"'))
@then(parsers.parse('user of {browser_id} sees that new share named '
                    '"{share_name}" was created from '
                    '{item_type} "{item_name}"'))
def create_share(browser_id, share_name, item_type, item_name, tmp_memory):
    assert item_type in ('file', 'directory')
    cur_dir = tmp_memory[browser_id]['website']['current_dir']
    fs.mkshare(browser_id, share_name, cur_dir.files[item_name], tmp_memory)


@when(parsers.parse('user of {browser_id} does not see any share info'))
@then(parsers.parse('user of {browser_id} does not see any share info'))
def check_that_there_is_no_share_info(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    assert not driver.find_elements_by_css_selector('#content-scroll '
                                                    '.share-info-head')


@when(parsers.parse('user of {browser_id} sees valid share info '
                    'for "{share_name}"'))
@then(parsers.parse('user of {browser_id} sees valid share info '
                    'for "{share_name}"'))
def check_share_info(selenium, browser_id, share_name, tmp_memory):
    driver = select_browser(selenium, browser_id)
    share = tmp_memory[browser_id]['shares'][share_name]

    name, breadcrumbs = Wait(driver, WAIT_FRONTEND).until(
        lambda d: d.find_elements_by_css_selector('#content-scroll '
                                                  '.share-info-head '
                                                  '.share-name, '
                                                  '#content-scroll '
                                                  '.share-info-head '
                                                  '.file-breadcrumbs-list'),
        message='waiting for share {:s} info to appear'.format(share_name)
    )
    assert name.text == share_name
    displayed_path = breadcrumbs.text.split()
    for dir1, dir2 in zip(displayed_path, fs.get_path(share.shared)):
        assert dir1 == dir2

    tmp_memory[browser_id]['website']['current_dir'] = share.shared


# TODO VFS-2634
@when(parsers.parse('user of {browser_id} sees that share received from '
                    'user of {browser_id2} is named "{share_name}"'))
@then(parsers.parse('user of {browser_id} sees that share is named '
                    '"{share_name}"'))
def check_share_name_in_public_view(selenium, browser_id, browser_id2,
                                    share_name, tmp_memory):
    driver = select_browser(selenium, browser_id)
    name = driver.find_element_by_css_selector('.share-name').text
    assert name == share_name
    share_root = tmp_memory[browser_id2]['shares'][share_name]
    tmp_memory[browser_id] = {'website': {'current_dir': share_root.shared}}


@when(parsers.parse('user of {browser_id} sees that current working directory '
                    'is {path}'))
@then(parsers.parse('user of {browser_id} sees that current working directory '
                    'is {path}'))
def check_breadcrumbs(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    path = path.split('/')
    breadcrumbs = driver.find_element_by_css_selector('.files-list '
                                                      '.file-breadcrumbs-list')
    breadcrumbs_path = breadcrumbs.text.split()
    for dir1, dir2 in zip(path, breadcrumbs_path):
        assert dir1 == dir2


# TODO VFS-2634
@when(parsers.parse('user of {browser_id} sees that the "{name}" has '
                    'disappeared from the shared list'))
@then(parsers.parse('user of {browser_id} sees that the "{name}" has '
                    'disappeared from the shared list'))
def check_if_renamed(selenium, browser_id, name):
    driver = select_browser(selenium, browser_id)

    def _check_for_lack_of_item_in_list(s):
        items = s.find_elements_by_css_selector('.shares-list .secondary-'
                                                'sidebar-item .item-label '
                                                '.truncate')
        return all(item.text != name for item in items)

    def _refresh_and_call():
        """Refresh browser and keep calling callback with given args
        until achieve expected result or timeout.
        """
        from tests.gui.utils.generic import parse_url
        from tests.gui.steps.oneprovider_common import _click_on_tab_in_main_menu_sidebar
        op_url = parse_url(driver.current_url).group('base_url')
        driver.get(op_url)
        _click_on_tab_in_main_menu_sidebar(driver, 'shared')

        try:
            result = Wait(driver, WAIT_REFRESH).until(
                lambda s: _check_for_lack_of_item_in_list(s)
            )
        except TimeoutException:
            return None
        else:
            return result

    driver = select_browser(selenium, browser_id)
    Wait(driver, MAX_REFRESH_COUNT*WAIT_BACKEND).until(
        lambda s: _refresh_and_call(),
        message='waiting for {:s} to disappear from '
                'groups list'.format(name)
    )


# TODO VFS-2634
@when(parsers.parse('user of {browser_id} sees that '
                    '"{prev_name}" has been renamed to "{next_name}"'))
@then(parsers.parse('user of {browser_id} sees that '
                    '"{prev_name}" has been renamed to "{next_name}"'))
def check_if_renamed(selenium, browser_id, prev_name, next_name, tmp_memory):
    from tests.gui.steps.oneprovider_common import _check_for_item_in_given_list
    driver = select_browser(selenium, browser_id)

    def _check_for_lack_of_item_in_list(s):
        items = s.find_elements_by_css_selector('.shares-list .secondary-'
                                                'sidebar-item .item-label '
                                                '.truncate')
        return all(item.text != prev_name for item in items)

    def _refresh_and_call():
        """Refresh browser and keep calling callback with given args
        until achieve expected result or timeout.
        """
        from tests.gui.utils.generic import parse_url
        from tests.gui.steps.oneprovider_common import _click_on_tab_in_main_menu_sidebar
        op_url = parse_url(driver.current_url).group('base_url')
        driver.get(op_url)
        _click_on_tab_in_main_menu_sidebar(driver, 'shared')

        try:
            result = Wait(driver, WAIT_REFRESH).until(
                lambda s: _check_for_lack_of_item_in_list(s)
            )
        except TimeoutException:
            return None
        else:
            return result

    driver = select_browser(selenium, browser_id)
    Wait(driver, MAX_REFRESH_COUNT*WAIT_BACKEND).until(
        lambda s: _refresh_and_call(),
        message='waiting for {:s} to disappear from '
                'groups list'.format(prev_name)
    )

    _check_for_item_in_given_list(driver, next_name, 'shares')

    share = tmp_memory[browser_id]['shares'].pop(prev_name, None)
    tmp_memory[browser_id]['shares'][next_name] = share


@when(parsers.parse('user of {browser_id} sees that he '
                    'no longer has access to share'))
@then(parsers.parse('user of {browser_id} sees that he '
                    'no longer has access to share'))
def check_if_user_lost_access(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    old_page = driver.find_element_by_css_selector('html')
    Wait(driver, WAIT_BACKEND).until(
        staleness_of(old_page)
    )
    assert not re.search(r'https?://.*?/public/shares(/.*)?', driver.current_url)
