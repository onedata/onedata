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
from pytest_bdd import when, then, parsers, given
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains

from ..utils.inspect import selector
from ..utils.generic import find_item_with_given_properties, refresh_and_call, \
    click_on_element
from pytest_selenium_multi.pytest_selenium_multi import select_browser


@when(parsers.re(r'user of (?P<browser_id>.+) uses spaces select to change '
                 r'data space to "(?P<space_name>.+)"'))
def change_space(selenium, browser_id, space_name):
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


@then(parsers.parse('user of {browser_id} double clicks '
                    'on file "{file_name}" from files list'))
@when(parsers.parse('user of {browser_id} double clicks '
                    'on file "{file_name}" from files list'))
def op_select_file_from_file_list(selenium, browser_id, file_name):
    driver = select_browser(selenium, browser_id)
    check_properties = selector(driver, text=file_name,
                                check_visibility=True)
    css_path = '.files-list table.files-table td.file-list-col-file'

    list_item = Wait(driver, WAIT_FRONTEND).until(
        lambda s: find_item_with_given_properties(s, css_path,
                                                  check_properties),
        message='searching for {:s} in file list'.format(file_name)
    )
    ActionChains(driver).double_click(list_item).perform()


@then(parsers.parse('user of {browser_id} sees that downloaded file '
                    '"{file_name}" contains "{content}"'))
def check_if_downloaded_file_contains_given_content(selenium, tmpdir,
                                                    file_name,
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


def _check_for_lack_of_file_in_file_list(driver, file_name):
    def _find_file(s, name):
        files = s.find_elements_by_css_selector('.files-list td')
        return all(li.text != name for li in files)

    Wait(driver, MAX_REFRESH_COUNT * WAIT_BACKEND).until(
        lambda s: refresh_and_call(s, _find_file,
                                   file_name),
        message='searching for lack of {:s} ''on file list'.format(file_name)
    )


@given(parsers.parse('that in {browser_id} there is no file named '
                     '"{file_list_elem}" in files list'))
@given(parsers.parse('that in {browser_id} there is no directory named '
                     '"{file_list_elem}" in files list'))
def check_if_file_not_exist(selenium, browser_id, file_list_elem):
    driver = select_browser(selenium, browser_id)
    _check_for_lack_of_file_in_file_list(driver, file_list_elem)


@then(parsers.parse('user of {browser_id} should not see directory named '
                    '"{file_list_elem}" in files list'))
@then(parsers.parse('user of {browser_id} should not see file named '
                    '"{file_list_elem}" in files list'))
def check_absence_deleted_file(selenium, browser_id, file_list_elem):
    driver = select_browser(selenium, browser_id)
    _check_for_lack_of_file_in_file_list(driver, file_list_elem)


def _check_for_file_in_file_list(driver, file_name):
    def _find_file(s, name):
        files = s.find_elements_by_css_selector('.files-list td')
        return sum(1 for li in files if li.text == name) == 1

    Wait(driver, MAX_REFRESH_COUNT * WAIT_BACKEND).until(
        lambda s: refresh_and_call(s, _find_file,
                                   file_name),
        message='searching for exactly one {:s} on file list'.format(file_name)
    )


@given(parsers.parse('that in {browser_id} there is a "{file_name}" file '
                     'on the files list'))
def existing_file(selenium, browser_id, file_name):
    driver = select_browser(selenium, browser_id)
    _check_for_file_in_file_list(driver, file_name)


@when(parsers.parse('user of {browser_id} sees new file named '
                    '"{file_list_element}" in files list'))
@then(parsers.parse('user of {browser_id} sees new directory named '
                    '"{file_list_element}" in files list'))
@then(parsers.parse('user of {browser_id} sees new file named '
                    '"{file_list_element}" in files list'))
def op_check_if_new_file_appeared(selenium, browser_id, file_list_element):
    driver = select_browser(selenium, browser_id)
    _check_for_file_in_file_list(driver, file_list_element)


@then(parsers.parse('user of {browser_id} clicks the button from top menu bar '
                    'with tooltip "{tooltip_name}"'))
@when(parsers.parse('user of {browser_id} clicks the button from top menu bar '
                    'with tooltip "{tooltip_name}"'))
def op_click_tooltip_from_top_menu_bar(selenium, browser_id, tooltip_name):

    def _find_tooltip_with_given_name(s):
        tooltips = s.find_elements_by_css_selector('ul.toolbar-group a')
        for tooltip in tooltips:
            if tooltip.get_attribute('data-original-title') == tooltip_name:
                return tooltip

    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_BACKEND).until(
        _find_tooltip_with_given_name,
        message='clicking on {:s} from top menu bar'.format(tooltip_name)
    ).click()


@then(parsers.parse('user of {browser_id} selects "{file_list_element}" '
                    'from files list'))
@when(parsers.parse('user of {browser_id} selects "{file_list_element}" '
                    'from files list'))
def op_select_file(selenium, browser_id, file_list_element):
    driver = select_browser(selenium, browser_id)
    click_on_element(driver, item_name=file_list_element,
                     css_path='.files-list td',
                     ignore_case=False,
                     msg='clicking on {:s} in file '
                         'list'.format(file_list_element))


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
