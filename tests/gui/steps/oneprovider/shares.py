"""This module contains gherkin steps to run acceptance tests featuring
shares in oneprovider web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")

import re

from pytest_bdd import when, then, parsers

from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND
from tests.gui.utils.generic import repeat_failed


def _check_share_presence(driver, op_page, share_name):
    shares = {share.name for share
              in op_page(driver).shares.sidebar.shares}
    return share_name in shares


@when(parsers.parse('user of {browser_id} selects "{share_name}" '
                    'from shares sidebar list'))
@then(parsers.parse('user of {browser_id} selects "{share_name}" '
                    'from shares sidebar list'))
def select_share_from_sidebar_list(selenium, browser_id, share_name, op_page):
    op_page(selenium[browser_id]).shares.sidebar.shares[share_name].click()


@when(parsers.parse('user of {browser_id} sees that share named '
                    '"{name}" has appeared in the shared list'))
@then(parsers.parse('user of {browser_id} sees that share named '
                    '"{name}" has appeared in the shared list'))
@repeat_failed(timeout=WAIT_BACKEND)
def is_present_on_share_list(selenium, browser_id, name, op_page):
    assert _check_share_presence(selenium[browser_id], op_page, name) is True, \
        'no share named "{}" found in shares sidebar'.format(name)


@when(parsers.parse('user of {browser_id} sees that share named '
                    '"{name}" has disappeared from the shares list'))
@then(parsers.parse('user of {browser_id} sees that share named '
                    '"{name}" has disappeared from the shares list'))
@repeat_failed(timeout=WAIT_BACKEND)
def is_not_present_in_share_list(selenium, browser_id, name, op_page):
    assert _check_share_presence(selenium[browser_id], op_page, name) is False, \
        ('share named "{}" found in shares sidebar, '
         'while it should not be'.format(name))


@when(parsers.parse('user of {browser_id} sees that '
                    '"{prev_name}" has been renamed to "{next_name}"'))
@then(parsers.parse('user of {browser_id} sees that '
                    '"{prev_name}" has been renamed to "{next_name}"'))
@repeat_failed(timeout=WAIT_BACKEND)
def has_share_been_renamed(selenium, browser_id, prev_name, next_name, op_page):
    assert _check_share_presence(selenium[browser_id], op_page,
                                 prev_name) is False, \
        ('share named "{}" found in shares sidebar, '
         'while it should not be'.format(prev_name))

    assert _check_share_presence(selenium[browser_id], op_page,
                                 next_name) is True, \
        'no share named "{}" found in shares sidebar'.format(next_name)


@when(parsers.parse('user of {browser_id} sees that absolute share path '
                    'visible in share\'s info header is as follows: {path}'))
@then(parsers.parse('user of {browser_id} sees that absolute share path '
                    'visible in share\'s info header is as follows: {path}'))
@repeat_failed(timeout=WAIT_FRONTEND)
def is_share_abs_path_correct(selenium, browser_id, path, op_page):
    displayed_path = op_page(selenium[browser_id]).shares.path.pwd()
    assert displayed_path == path, \
        ('displayed share absolute path is {} '
         'instead of expected {}'.format(displayed_path, path))


@when(parsers.parse('user of {browser_id} sees that current working directory '
                    'path visible in share\'s file browser is as follows: {cwd}'))
@then(parsers.parse('user of {browser_id} sees that current working directory '
                    'path visible in share\'s file browser is as follows: {cwd}'))
@repeat_failed(timeout=WAIT_FRONTEND)
def is_cwd_correct(selenium, browser_id, cwd, op_page):
    displayed_cwd = op_page(selenium[browser_id]).shares.breadcrumbs.pwd()
    assert displayed_cwd == cwd, \
        ('displayed share cwd in file browser is {} '
         'instead of expected {}'.format(displayed_cwd, cwd))


@when(parsers.parse('user of {browser_id} changes current working directory '
                    'to {path} using breadcrumbs from share\'s file browser'))
@then(parsers.parse('user of {browser_id} changes current working directory '
                    'to {path} using breadcrumbs from share\'s file browser'))
@repeat_failed(timeout=WAIT_FRONTEND)
def change_cwd_using_breadcrumbs(selenium, browser_id, path, op_page):
    op_page(selenium[browser_id]).shares.breadcrumbs.chdir(path)


@when(parsers.parse('user of {browser_id} clicks on {path} '
                    'using breadcrumbs from share info header'))
@then(parsers.parse('user of {browser_id} clicks on {path} '
                    'using breadcrumbs from share\'s info header'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_on_dir_in_abs_path(selenium, browser_id, path, op_page):
    op_page(selenium[browser_id]).shares.path.chdir(path)


@when(parsers.parse('user of {browser_id} sees that selected share '
                    'is named "{share_name}"'))
@then(parsers.parse('user of {browser_id} sees that selected share '
                    'is named "{share_name}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def is_selected_share_named(selenium, browser_id, share_name, op_page):
    displayed_name = op_page(selenium[browser_id]).shares.name
    assert displayed_name == share_name, \
        ('displayed share name is "{}" instead of '
         'expected "{}"'.format(displayed_name, share_name))


@when(parsers.parse('user of {browser_id} sees that '
                    'public share is named "{share_name}"'))
@then(parsers.parse('user of {browser_id} sees that '
                    'public share is named "{share_name}"'))
@repeat_failed(timeout=WAIT_BACKEND, interval=0.5)
def is_public_share_named(selenium, browser_id, share_name, public_share):
    driver = selenium[browser_id]
    displayed_name = public_share(driver).name
    if displayed_name != share_name:
        driver.refresh()
        raise RuntimeError('displayed public share name is "{}" instead of '
                           'expected "{}"'.format(displayed_name, share_name))


@when(parsers.parse('user of {browser_id} does not see any share'))
@then(parsers.parse('user of {browser_id} does not see any share'))
@repeat_failed(timeout=WAIT_FRONTEND)
def is_not_any_share(selenium, browser_id, op_page):
    assert len(op_page(selenium[browser_id]).shares.sidebar.shares) == 0, \
        'shares found, but there should not be any'


@when(parsers.parse('user of {browser_id} sees that he '
                    'no longer can view the share'))
@then(parsers.parse('user of {browser_id} sees that he '
                    'no longer can view the share'))
@repeat_failed(timeout=WAIT_BACKEND)
def is_share_not_viewable(selenium, browser_id):
    url = selenium[browser_id].current_url
    assert not re.search(r'https?://.*?/public/shares(/.*)?', url), \
        r'user can see public share with url {}'.format(url)


@when(parsers.parse('user of {browser_id} sees file browser '
                    'in shared tab in Oneprovider page'))
@then(parsers.parse('user of {browser_id} sees file browser '
                    'in shared tab in Oneprovider page'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_file_browser_in_shared_tab_in_op(selenium, browser_id,
                                            op_page, tmp_memory):
    driver = selenium[browser_id]
    file_browser = op_page(driver).shares.file_browser
    tmp_memory[browser_id]['file_browser'] = file_browser


@when(parsers.parse('user of {browser_id} clicks on settings icon displayed '
                    'for "{share_name}" item on the shares sidebar list'))
@then(parsers.parse('user of {browser_id} clicks on settings icon displayed '
                    'for "{share_name}" item on the shares sidebar list'))
def click_settings_icon_for_share(selenium, browser_id, share_name, op_page):
    (op_page(selenium[browser_id])
     .shares
     .sidebar
     .shares[share_name]
     .settings
     .expand())


@when(parsers.parse('user of {browser_id} clicks on the "{option_name}" item '
                    'in settings dropdown for share named "{share_name}"'))
@then(parsers.parse('user of {browser_id} clicks on the "{option_name}" item '
                    'in settings dropdown for share named "{share_name}"'))
def click_on_item_in_share_settings_dropdown(selenium, browser_id, option_name,
                                             share_name, op_page):
    (op_page(selenium[browser_id])
     .shares
     .sidebar
     .shares[share_name]
     .settings
     .options[option_name]
     .click())


@when(parsers.parse('user of {browser_id} sees file browser '
                    'in public share view'))
@then(parsers.parse('user of {browser_id} sees file browser '
                    'in public share view'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_file_browser_in_public_share(selenium, browser_id,
                                        public_share, tmp_memory):
    file_browser = public_share(selenium[browser_id]).file_browser
    tmp_memory[browser_id]['file_browser'] = file_browser


@when(parsers.parse('user of {browser_id} sees that current working directory '
                    'path visible in public share\'s file browser '
                    'is as follows: {cwd}'))
@then(parsers.parse('user of {browser_id} sees that current working directory '
                    'path visible in public share\'s file browser '
                    'is as follows: {cwd}'))
@repeat_failed(timeout=WAIT_FRONTEND)
def is_public_share_cwd_correct(selenium, browser_id, cwd, public_share):
    displayed_cwd = public_share(selenium[browser_id]).breadcrumbs.pwd()
    assert displayed_cwd == cwd, \
        ('displayed share cwd in file browser is {} '
         'instead of expected {}'.format(displayed_cwd, cwd))


@when(parsers.parse('user of {browser_id} changes current working directory '
                    'to {path} using breadcrumbs from public share\'s '
                    'file browser'))
@then(parsers.parse('user of {browser_id} changes current working directory '
                    'to {path} using breadcrumbs from public share\'s '
                    'file browser'))
@repeat_failed(timeout=WAIT_FRONTEND)
def change_public_share_cwd_using_breadcrumbs(selenium, browser_id, path,
                                              public_share):
    public_share(selenium[browser_id]).breadcrumbs.chdir(path)
