# coding=utf-8
"""Steps used for handling of data tab elements (e.g. toolbar)
in various GUI testing scenarios
"""

from pytest_bdd import when, then, parsers

from pytest_selenium_multi.pytest_selenium_multi import select_browser

from tests.gui.conftest import WAIT_BACKEND, SELENIUM_IMPLICIT_WAIT
from tests.gui.utils.generic import repeat, implicit_wait

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@when(parsers.parse('user of {browser_id} uses spaces select to change '
                    'data space to "{space_name}"'))
@then(parsers.parse('user of {browser_id} uses spaces select to change '
                    'data space to "{space_name}"'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def change_space_view_in_data_tab_in_op(selenium, browser_id,
                                        space_name, op_page):
    driver = select_browser(selenium, browser_id)
    selector = op_page(driver).data.sidebar.space_selector
    selector.expand()
    selector[space_name].click()


@then(parsers.parse('user of {browser_id} clicks the button from top menu bar '
                    'with tooltip "{tooltip_name}"'))
@when(parsers.parse('user of {browser_id} clicks the button from top menu bar '
                    'with tooltip "{tooltip_name}"'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def click_tooltip_from_toolbar_in_data_tab_in_op(selenium, browser_id,
                                                 tooltip_name, op_page):
    driver = select_browser(selenium, browser_id)
    op_page(driver).data.toolbar[tooltip_name].click()


@when(parsers.parse('user of {browser_id} sees that current working directory '
                    'displayed in breadcrumbs is {path}'))
@then(parsers.parse('user of {browser_id} sees that current working directory '
                    'displayed in breadcrumbs is {path}'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def is_displayed_breadcrumbs_in_data_tab_in_op_correct(selenium, browser_id,
                                                       path, op_page):
    driver = select_browser(selenium, browser_id)
    breadcrumbs = op_page(driver).data.breadcrumbs.pwd()
    assert path == breadcrumbs, \
        'expected breadcrumbs {}; displayed: {}'.format(path, breadcrumbs)


@when(parsers.parse('user of {browser_id} changes current working directory '
                    'to {path} using breadcrumbs'))
@then(parsers.parse('user of {browser_id} changes current working directory '
                    'to {path} using breadcrumbs'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def change_cwd_using_breadcrumbs_in_data_tab_in_op(selenium, browser_id,
                                                   path, op_page):
    driver = select_browser(selenium, browser_id)
    op_page(driver).data.breadcrumbs.chdir(path)


@when(parsers.parse('user of {browser_id} sees that current working directory '
                    'displayed in directory tree is {path}'))
@then(parsers.parse('user of {browser_id} sees that current working directory '
                    'displayed in directory tree is {path}'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def is_displayed_dir_tree_in_data_tab_in_op_correct(selenium, browser_id,
                                                    path, op_page):
    driver = select_browser(selenium, browser_id)
    with implicit_wait(driver, 0.05, SELENIUM_IMPLICIT_WAIT):
        pwd = op_page(driver).data.sidebar.cwd.pwd()
    assert path == pwd, 'expected path {}\n got: {}'.format(path, pwd)


@when(parsers.parse('user of {browser_id} changes current working directory '
                    'to {path} using directory tree'))
@then(parsers.parse('user of {browser_id} changes current working directory '
                    'to {path} using directory tree'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def change_cwd_using_dir_tree_in_data_tab_in_op(selenium, browser_id,
                                                path, op_page):
    driver = select_browser(selenium, browser_id)
    cwd = op_page(driver).data.sidebar.root_dir
    cwd.click()
    for directory in (dir for dir in path.split('/') if dir != ''):
        cwd = cwd[directory]
        cwd.click()
