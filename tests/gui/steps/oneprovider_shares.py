"""Steps for features of Oneprovider shares.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


import re

from tests.gui.conftest import WAIT_BACKEND
from pytest_bdd import when, then, parsers
from selenium.webdriver.support.ui import WebDriverWait as Wait
from pytest_selenium_multi.pytest_selenium_multi import select_browser
from selenium.webdriver.support.expected_conditions import staleness_of


def _get_share_from_shares_list(driver, name):
    return driver.find_elements_by_css_selector('.shares-list '
                                                '.secondary-sidebar-item '
                                                '.item-label[title="{name}"'
                                                ''.format(name=name))


@when(parsers.parse('user of {browser_id} sees that share named '
                    '"{name}" has appeared in the shared list'))
@then(parsers.parse('user of {browser_id} sees that share named '
                    '"{name}" has appeared in the shared list'))
def is_present_on_share_list(selenium, browser_id, name):
    driver = select_browser(selenium, browser_id)
    assert len(_get_share_from_shares_list(driver, name)) == 1


@when(parsers.parse('user of {browser_id} sees that share named '
                    '"name" has disappeared from the shared list'))
@then(parsers.parse('user of {browser_id} sees that share named '
                    '"name" has disappeared from the shared list'))
def is_not_present_in_share_list(selenium, browser_id, name):
    driver = select_browser(selenium, browser_id)
    assert len(_get_share_from_shares_list(driver, name)) == 0


@when(parsers.parse('user of {browser_id} selects share named '
                    '"{name}" from the shared list'))
@then(parsers.parse('user of {browser_id} selects share named '
                    '"{name}" from the shared list'))
def select_share_from_share_list(selenium, browser_id, name, tmp_memory):
    driver = select_browser(selenium, browser_id)
    share = _get_share_from_shares_list(driver, name)
    tmp_memory[browser_id].update({'share': share})


@when(parsers.parse('user of {browser_id} sees that '
                    '"{prev_name}" has been renamed to "{next_name}"'))
@then(parsers.parse('user of {browser_id} sees that '
                    '"{prev_name}" has been renamed to "{next_name}"'))
def has_share_been_renamed(selenium, browser_id, prev_name, next_name):
    driver = select_browser(selenium, browser_id)
    assert len(_get_share_from_shares_list(driver, prev_name)) == 0
    assert len(_get_share_from_shares_list(driver, next_name)) == 1


@when(parsers.parse('user of {browser_id} sees that '
                    'absolute share path is {path}'))
@then(parsers.parse('user of {browser_id} sees that '
                    'absolute share path is {path}'))
def has_share_absolute_path(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    abs_path = driver.find_element_by_css_selector('.share-info-head '
                                                   '.file-breadcrumbs-list')
    abs_path = abs_path.text.split()
    path = path.split('/')
    for dir1, dir2 in zip(path, abs_path):
        assert dir1 == dir2


@when(parsers.parse('user of {browser_id} sees that '
                    'current working directory is {path}'))
@when(parsers.parse('user of {browser_id} sees that '
                    'current working directory is {path}'))
def has_cwd(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    cwd = driver.find_element_by_css_selector('.files-list '
                                              '.file-breadcrumbs-list')
    cwd = cwd.text.split()
    path = path.split('/')
    for dir1, dir2 in zip(path, cwd):
        assert dir1 == dir2


@when(parsers.parse('user of {browser_id} sees that selected share '
                    'is named "{share_name}"'))
@then(parsers.parse('user of {browser_id} sees that selected share '
                    'is named "{share_name}"'))
def is_share_named(selenium, browser_id, share_name):
    driver = select_browser(selenium, browser_id)
    name = driver.find_element_by_css_selector('#content-scroll '
                                               '.share-info-head '
                                               '.share-name').text
    assert name == share_name


@when(parsers.parse('user of {browser_id} does not see any share info'))
@then(parsers.parse('user of {browser_id} does not see any share info'))
def is_not_any_share_info(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    shares = driver.find_elements_by_css_selector('.shares-list '
                                                  '.secondary-sidebar-item, '
                                                  '#content-scroll '
                                                  '.share-info-head')
    assert len(shares) == 0


@when(parsers.parse('user of {browser_id} sees that '
                    'public share is named "{share_name}"'))
@then(parsers.parse('user of {browser_id} sees that '
                    'public share is named "{share_name}"'))
def is_public_share_named(selenium, browser_id, share_name):
    driver = select_browser(selenium, browser_id)
    name = driver.find_element_by_css_selector('.share-name').text
    assert name == share_name


@when(parsers.parse('user of {browser_id} sees that he '
                    'no longer has access to the share'))
@then(parsers.parse('user of {browser_id} sees that he '
                    'no longer has access to the share'))
def check_if_user_lost_access(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    old_page = driver.find_element_by_css_selector('html')
    Wait(driver, WAIT_BACKEND).until(
        staleness_of(old_page)
    )
    assert not re.search(r'https?://.*?/public/shares(/.*)?',
                         driver.current_url)


# TODO merge with file list
@when(parsers.parse('user of {browser_id} clicks on '
                    'share icon next to directory named "{name}"'))
@then(parsers.parse('user of {browser_id} clicks on '
                    'share icon next to directory named "{name}"'))
def click_on_share_icon(selenium, browser_id, name):
    driver = select_browser(selenium, browser_id)
    files = driver.find_elements_by_css_selector('.files-table '
                                                 'td .file-label, '
                                                 '.files-table '
                                                 'td .file-row-tools')
    for file_name, icon in zip(files[::2], files[1::2]):
        if file_name.text == name:
            icon.find_element_by_css_selector('.oneicon-share').click()
