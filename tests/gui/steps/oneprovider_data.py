"""Steps for features of Onezone login page.
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
import os
import time

from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND, MAX_REFRESH_COUNT
from tests.gui.utils.generic import upload_file_path
from pytest_bdd import when, then, parsers
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains

from tests.gui.utils.generic import refresh_and_call
from pytest_selenium_multi.pytest_selenium_multi import select_browser


import tests.gui.utils.file_system as fs
from tests.utils.acceptance_utils import list_parser


@when(parsers.re(r'user of (?P<browser_id>.+) uses spaces select to change '
                 r'data space to "(?P<space_name>.+)"'))
@then(parsers.re(r'user of (?P<browser_id>.+) uses spaces select to change '
                 r'data space to "(?P<space_name>.+)"'))
def change_space(selenium, browser_id, space_name, tmp_memory):
    driver = select_browser(selenium, browser_id)
    # HACK: because Firefox driver have buggy EC.element_to_be_clickable,
    # we wait for loader to disappear
    Wait(driver, WAIT_FRONTEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR, '.common-loader-spinner'))
    )
    Wait(driver, WAIT_FRONTEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, '.data-spaces-select a[data-toggle=dropdown]'))
    ).click()
    spaces = driver.find_elements_by_css_selector('.data-spaces-select .dropdown-menu a')

    def space_by_name(_):
        named_spaces = [s for s in spaces if re.match(space_name, s.text.strip(), re.I)]
        if len(named_spaces) > 0 and named_spaces[0].is_enabled():
            return named_spaces[0]
        else:
            return None

    Wait(driver, WAIT_FRONTEND).until(space_by_name).click()

    spaces = tmp_memory[browser_id]['spaces']
    if space_name in spaces:
        root_dir = spaces[space_name]
    else:
        root_dir = spaces[space_name] = fs.mkdir(space_name)

    tmp_memory[browser_id]['website']['current_dir'] = root_dir


@when(parsers.parse('user of {browser_id} uses upload button in toolbar '
                    'to upload file "{file_name}" to current dir'))
def upload_file_to_current_dir(selenium, browser_id, file_name):
    """This interaction is very hacky, because uploading files with Selenium
    needs to use input element, but we do not use it directly in frontend.
    So we unhide an input element for a while and pass a local file path to it.
    """
    driver = select_browser(selenium, browser_id)
    # HACK: for Firefox driver - because we cannot interact with hidden elements
    driver.execute_script("$('input#toolbar-file-browse').removeClass('hidden')")
    driver.find_element_by_css_selector('input#toolbar-file-browse').send_keys(
        upload_file_path(file_name)
    )
    driver.execute_script("$('input#toolbar-file-browse').addClass('hidden')")

    def file_browser_ready(d):
        files_table = d.find_element_by_css_selector('.files-table')
        return not re.match(r'.*is-loading.*', files_table.get_attribute('class'))

    Wait(driver, WAIT_BACKEND).until(file_browser_ready)


def _get_items_from_file_list(driver, name, type):
    files = driver.find_elements_by_css_selector('table.files-table td '
                                                 '.file-icon .oneicon, '
                                                 'table.files-table td '
                                                 '.file-label')
    icons, labels = files[::2], files[1::2]
    return [label for icon, label in zip(icons, labels)
            if label.text == name and type in icon.get_attribute('class')]


@then(parsers.parse('user of {browser_id} double clicks '
                    'on {item_type} "{item_name}" from files list'))
@when(parsers.parse('user of {browser_id} double clicks '
                    'on {item_type} "{item_name}" from files list'))
def double_click_on_item(selenium, browser_id, item_name,
                         item_type, tmp_memory):
    driver = select_browser(selenium, browser_id)
    _, _, icon = get_icon_and_fun_for_item_type(item_type)

    item = Wait(driver, WAIT_FRONTEND).until(
        lambda d: _get_items_from_file_list(d, item_name, item_type),
        message='searching for {:s} in file list'.format(item_name)
    )[0]
    ActionChains(driver).double_click(item).perform()
    if item_type == 'directory':
        curr_dir = tmp_memory[browser_id]['website']['current_dir']
        tmp_memory[browser_id]['website']['current_dir'] = curr_dir.files[item_name]


@then(parsers.parse('user of {browser_id} sees that downloaded file '
                    '"{file_name}" contains "{content}"'))
def has_downloaded_file_content(selenium, tmpdir, file_name,
                                content, browser_id):
    driver = select_browser(selenium, browser_id)
    tmpdir_path = str(tmpdir)
    file_path = os.path.join(tmpdir_path, file_name)

    # sleep waiting for file to finish downloading
    for sleep_time in range(10):
        if not os.listdir(tmpdir_path):
            time.sleep(sleep_time)

    def _check_file_content():
        with open(file_path, 'r') as f:
            file_content = ''.join(f.readlines())
            return content == file_content

    Wait(driver, WAIT_BACKEND).until(
        lambda _: _check_file_content,
        message='checking if downloaded file contains {:s}'.format(content)
    )


def get_icon_and_fun_for_item_type(item_type):
    return {'directory-share': (fs.mkshare, fs.rmshare,
                                'oneicon-folder-share'),
            'directory': (fs.mkdir, fs.rmdir, 'oneicon-folder'),
            'file': (fs.touch, fs.rmfile, 'oneicon-file')}[item_type]


def _not_in_file_list(driver, item_name, item_type):
    def _not_in(d, name):
        return not _get_items_from_file_list(d, name, item_type)

    return Wait(driver, MAX_REFRESH_COUNT * WAIT_BACKEND).until(
        lambda s: refresh_and_call(s, _not_in,
                                   item_name),
        message='searching for lack of {:s} ''on file list'.format(item_name)
    )


@when(parsers.parse('user of {browser_id} sees that {item_type} '
                    'named "{item_name}" has disappeared from file list'))
@then(parsers.parse('user of {browser_id} sees that {item_type} '
                    'named "{item_name}" has disappeared from file list'))
def op_check_if_item_disappeared_from_file_list(selenium, browser_id,
                                                item_type, item_name,
                                                tmp_memory):
    driver = select_browser(selenium, browser_id)
    _, rm_fun, icon = get_icon_and_fun_for_item_type(item_type)
    assert _not_in_file_list(driver, item_name, icon)
    cur_dir = tmp_memory[browser_id]['website']['current_dir']
    rm_fun(item_name, cur_dir)


@when(parsers.parse('user of {browser_id} sees that shared {item_type} '
                    'named "{item_name}" has disappeared from file list'))
@then(parsers.parse('user of {browser_id} sees that shared {item_type} '
                    'named "{item_name}" has disappeared from file list'))
def op_check_if_item_disappeared_from_file_list(selenium, browser_id,
                                                item_type, item_name,
                                                tmp_memory):
    driver = select_browser(selenium, browser_id)
    _, rm_fun, icon = get_icon_and_fun_for_item_type('{:s}-share'
                                                     ''.format(item_type))
    assert _not_in_file_list(driver, item_name, icon)
    cur_dir = tmp_memory[browser_id]['website']['current_dir']
    rm_fun(browser_id, item_name, cur_dir, tmp_memory)


def _in_file_list(driver, item_name, item_type):
    def _in(d, name):
        return len(_get_items_from_file_list(d, name, item_type)) == 1

    return Wait(driver, MAX_REFRESH_COUNT * WAIT_BACKEND).until(
        lambda s: refresh_and_call(s, _in,
                                   item_name),
        message='searching for exactly one {} on file list'.format(item_name)
    )


@when(parsers.parse('user of {browser_id} sees that {item_type} '
                    'named "{item_name}" has appeared in file list'))
@then(parsers.parse('user of {browser_id} sees that {item_type} '
                    'named "{item_name}" has appeared in file list'))
def op_check_if_item_appeared_in_file_list(selenium, browser_id, item_type,
                                           item_name, tmp_memory):
    driver = select_browser(selenium, browser_id)
    mk_fun, _, icon = get_icon_and_fun_for_item_type(item_type)
    assert _in_file_list(driver, item_name, icon)
    cur_dir = tmp_memory[browser_id]['website']['current_dir']
    mk_fun(item_name, cur_dir)


@when(parsers.parse('user of {browser_id} sees that {item_type} '
                    'named "{item_name}" has became shared with alias '
                    '"{share_name}"'))
@then(parsers.parse('user of {browser_id} sees that {item_type} '
                    'named "{item_name}" has became shared with alias '
                    '"{share_name}"'))
def op_check_if_item_appeared_in_file_list(selenium, browser_id, item_type,
                                           item_name, share_name, tmp_memory):
    driver = select_browser(selenium, browser_id)
    mk_fun, _, icon = get_icon_and_fun_for_item_type('{:s}-share'
                                                     ''.format(item_type))
    assert _in_file_list(driver, item_name, icon)
    item = tmp_memory[browser_id]['website']['current_dir'].files[item_name]
    mk_fun(browser_id, share_name, item, tmp_memory)


@when(parsers.parse('user of {browser_id} sees that {item_type} '
                    'named "{item_name}" is no longer shared'))
@then(parsers.parse('user of {browser_id} sees that {item_type} '
                    'named "{item_name}" is no longer shared'))
def op_check_if_item_appeared_in_file_list(selenium, browser_id,
                                           item_type, item_name):
    driver = select_browser(selenium, browser_id)
    _, _, icon = get_icon_and_fun_for_item_type(item_type)
    assert _in_file_list(driver, item_name, icon)


@then(parsers.parse('user of {browser_id} clicks the button from top menu bar '
                    'with tooltip "{tooltip_name}"'))
@when(parsers.parse('user of {browser_id} clicks the button from top menu bar '
                    'with tooltip "{tooltip_name}"'))
def op_click_tooltip_from_top_menu_bar(selenium, browser_id, tooltip_name):
    driver = select_browser(selenium, browser_id)
    css_path = 'ul.toolbar-group a[data-original-title="{:s}"]' \
               ''.format(tooltip_name)
    Wait(driver, WAIT_BACKEND).until(
        lambda d: d.find_element_by_css_selector(css_path),
        message='clicking on button with tooltip {:s} '
                'from top menu bar'.format(tooltip_name)
    ).click()


@when(parsers.parse('user of {browser_id} selects {item_list} '
                    'from files list'))
@then(parsers.parse('user of {browser_id} selects {item_list} '
                    'from files list'))
def select_files_from_file_list(selenium, browser_id, item_list):
    driver = select_browser(selenium, browser_id)
    items = {item.text: item for item in
             driver.find_elements_by_css_selector('table.files-table '
                                                  'tr:not([class$="active"]) '
                                                  'td.file-list-col-file')}
    for item in list_parser(item_list):
        if item in items:
            item = items[item]
            Wait(driver, WAIT_FRONTEND).until(
                lambda _: item.is_displayed() and item.is_enabled(),
                message='clicking on {:s} in file list'.format(item.text)
            )
            item.click()


@then(parsers.parse('user of {browser_id} sees modal with name of provider '
                    'supporting space in providers column'))
def op_check_if_provider_name_is_in_tab(selenium, browser_id, tmp_memory):

    def _find_provider(s):
        providers = s.find_elements_by_css_selector(
            '#file-chunks-modal .container-fluid '
            'table.file-blocks-table td.provider-name')
        for elem in providers:
            if elem.text == tmp_memory[browser_id]['supporting_provider']:
                return elem
        return None

    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until(
        _find_provider,
        message='check file distribution, focusing on {:s} provide'
                ''.format(tmp_memory[browser_id]['supporting_provider'])
    )
