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
    return driver.find_elements_by_css_selector('ul.shares-list '
                                                '.secondary-sidebar-item '
                                                '.item-label[title="{name}"]'
                                                ''.format(name=name))


@when(parsers.parse('user of {browser_id} sees that share named '
                    '"{name}" has appeared in the shared list'))
@then(parsers.parse('user of {browser_id} sees that share named '
                    '"{name}" has appeared in the shared list'))
def is_present_on_share_list(selenium, browser_id, name):
    driver = select_browser(selenium, browser_id)
    assert len(_get_share_from_shares_list(driver, name)) == 1


@when(parsers.parse('user of {browser_id} sees that share named '
                    '"{name}" has disappeared from the shares list'))
@then(parsers.parse('user of {browser_id} sees that share named '
                    '"{name}" has disappeared from the shares list'))
def is_not_present_in_share_list(selenium, browser_id, name):
    driver = select_browser(selenium, browser_id)
    assert len(_get_share_from_shares_list(driver, name)) == 0


@when(parsers.parse('user of {browser_id} sees that '
                    '"{prev_name}" has been renamed to "{next_name}"'))
@then(parsers.parse('user of {browser_id} sees that '
                    '"{prev_name}" has been renamed to "{next_name}"'))
def has_share_been_renamed(selenium, browser_id, prev_name, next_name):
    driver = select_browser(selenium, browser_id)
    assert len(_get_share_from_shares_list(driver, prev_name)) == 0
    assert len(_get_share_from_shares_list(driver, next_name)) == 1


@when(parsers.parse('user of {browser_id} sees that absolute share path '
                    'visible in share\'s info header is as follows: {path}'))
@then(parsers.parse('user of {browser_id} sees that absolute share path '
                    'visible in share\'s info header is as follows: {path}'))
def is_share_abs_path_correct(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    abs_path = driver.find_element_by_css_selector('#content-scroll '
                                                   '.share-info-head '
                                                   '.file-breadcrumbs-list')
    for dir1, dir2 in zip(path.split('/'), abs_path.text.split()):
        assert dir1 == dir2


@when(parsers.parse('user of {browser_id} sees that current working directory '
                    'path visible in share\'s file browser is as follows: {path}'))
@then(parsers.parse('user of {browser_id} sees that current working directory '
                    'path visible in share\'s file browser is as follows: {path}'))
def is_cwd_correct(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    cwd = driver.find_element_by_css_selector('.files-list '
                                              '.file-breadcrumbs-list')
    for dir1, dir2 in zip(path.split('/'), cwd.text.split()):
        assert dir1 == dir2


@when(parsers.parse('user of {browser_id} changes current working directory '
                    'to {path} using breadcrumbs from share\'s file browser'))
@then(parsers.parse('user of {browser_id} changes current working directory '
                    'to {path} using breadcrumbs from share\'s file browser'))
def change_cwd_using_breadcrumbs(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    breadcrumbs = driver.find_elements_by_css_selector('.files-list '
                                                       '.file-breadcrumbs-list '
                                                       '.file-breadcrumbs-item '
                                                       'a')
    dir1, dir2 = None, None
    for dir1, dir2 in zip(path.split('/'), breadcrumbs):
        assert dir1 == dir2.text
    dir2.click()


@when(parsers.parse('user of {browser_id} clicks on {path} '
                    'using breadcrumbs from share info header'))
@then(parsers.parse('user of {browser_id} clicks on {path} '
                    'using breadcrumbs from share\'s info header'))
def click_on_dir_in_abs_path(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    breadcrumbs = driver.find_elements_by_css_selector('#content-scroll '
                                                       '.share-info-head '
                                                       '.file-breadcrumbs-list '
                                                       '.file-breadcrumbs-item '
                                                       'a')
    dir1, dir2 = None, None
    for dir1, dir2 in zip(path.split('/'), breadcrumbs):
        assert dir1 == dir2.text
    dir2.click()


@when(parsers.parse('user of {browser_id} sees that selected share '
                    'is named {share_name}'))
@then(parsers.parse('user of {browser_id} sees that selected share '
                    'is named {share_name}'))
def is_selected_share_named(selenium, browser_id, share_name):
    driver = select_browser(selenium, browser_id)
    name = driver.find_element_by_css_selector('#content-scroll '
                                               '.share-info-head '
                                               '.share-name').text
    assert name == share_name


@when(parsers.parse('user of {browser_id} sees that '
                    'public share is named {share_name}'))
@then(parsers.parse('user of {browser_id} sees that '
                    'public share is named {share_name}'))
def is_public_share_named(selenium, browser_id, share_name):
    driver = select_browser(selenium, browser_id)
    name = driver.find_element_by_css_selector('.share-name').text
    assert name == share_name


@when(parsers.parse('user of {browser_id} does not see any share'))
@then(parsers.parse('user of {browser_id} does not see any share'))
def is_not_any_share(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    shares = driver.find_elements_by_css_selector('.shares-list '
                                                  '.secondary-sidebar-item, '
                                                  '#content-scroll '
                                                  '.share-info-head')
    assert not shares


@when(parsers.parse('user of {browser_id} sees that he '
                    'no longer can view the share'))
@then(parsers.parse('user of {browser_id} sees that he '
                    'no longer can view the share'))
def is_share_not_viewable(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    old_page = driver.find_element_by_css_selector('html')
    Wait(driver, WAIT_BACKEND).until(
        staleness_of(old_page),
        message='waiting for public share view to disappear'
    )
    assert not re.search(r'https?://.*?/public/shares(/.*)?',
                         driver.current_url)
