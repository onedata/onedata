"""Steps for features of Onezone login page.
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import py

import re
import os
import time

from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND
from tests.gui.utils.generic import upload_file_path
from pytest_bdd import when, then, parsers, given
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from pytest_selenium_multi.pytest_selenium_multi import select_browser


@when(parsers.re(r'user of (?P<browser_id>.+) uses spaces select to change '
                 r'data space to "(?P<space_name>.+)"'))
@then(parsers.re(r'user of (?P<browser_id>.+) uses spaces select to change '
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


def _upload_files_to_cwd(driver, files):
    """This interaction is very hacky, because uploading files with Selenium
    needs to use input element, but we do not use it directly in frontend.
    So we unhide an input element for a while and pass a local file path to it.
    """
    # HACK: for Firefox driver - because we cannot interact with hidden elements
    driver.execute_script("$('input#toolbar-file-browse').removeClass('hidden')")
    driver.find_element_by_css_selector('input#toolbar-file-browse').send_keys(
        files
    )
    driver.execute_script("$('input#toolbar-file-browse').addClass('hidden')")

    upload_panel = driver.find_element_by_css_selector('.file-upload-panel')
    Wait(driver, WAIT_BACKEND).until_not(
        lambda _: upload_panel.is_displayed(),
        message='waiting for files to get uploaded'
    )


@when(parsers.parse('user of {browser_id} uses upload button in toolbar '
                    'to upload file "{file_name}" to current dir'))
def upload_file_to_cwd(selenium, browser_id, file_name):
    driver = select_browser(selenium, browser_id)
    _upload_files_to_cwd(driver, upload_file_path(file_name))


@when(parsers.parse('user of {browser_id} uses upload button in toolbar to '
                    'upload files from directory "{dir_path}" to current dir'))
def upload_files_to_cwd(selenium, browser_id, dir_path, tmpdir):
    driver = select_browser(selenium, browser_id)
    directory = tmpdir.ensure(browser_id, *dir_path.split('/'), dir=True)
    _upload_files_to_cwd(driver, '\n'.join(str(item) for item
                                           in directory.listdir()
                                           if item.isfile()))


@when(parsers.parse('user of {browser_id} sees that content of downloaded '
                    'file "{file_name}" is equal to: "{content}"'))
@then(parsers.parse('user of {browser_id} sees that content of downloaded '
                    'file "{file_name}" is equal to: "{content}"'))
def has_downloaded_file_content(selenium, tmpdir, file_name,
                                content, browser_id):
    driver = select_browser(selenium, browser_id)
    file_path = None

    # sleep waiting for file to finish downloading
    for sleep_time in range(10):
        try:
            file_path = tmpdir.ensure('download', file_name)
        except py.error.ENOENT:
            continue

    def _check_file_content():
        with file_path.open() as f:
            file_content = ''.join(f.readlines())
            return content == file_content

    assert file_path, 'file has not been downloaded'
    Wait(driver, WAIT_BACKEND).until(
        lambda _: _check_file_content(),
        message='checking if downloaded file contains {:s}'.format(content)
    )


@then(parsers.parse('user of {browser_id} clicks the button from top menu bar '
                    'with tooltip "{tooltip_name}"'))
@when(parsers.parse('user of {browser_id} clicks the button from top menu bar '
                    'with tooltip "{tooltip_name}"'))
def op_click_tooltip_from_top_menu_bar(selenium, browser_id, tooltip_name):
    driver = select_browser(selenium, browser_id)
    driver.find_element_by_css_selector('ul.toolbar-group '
                                        'a[data-original-title="{:s}"]'
                                        ''.format(tooltip_name)).click()


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


@when(parsers.parse('user of {browser_id} sees that current working directory '
                    'displayed in breadcrumbs is {path}'))
@then(parsers.parse('user of {browser_id} sees that current working directory '
                    'displayed in breadcrumbs is {path}'))
def is_displayed_path_correct(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    breadcrumbs = driver.find_element_by_css_selector('#main-content '
                                                      '.secondary-top-bar '
                                                      '.file-breadcrumbs-list')
    for dir1, dir2 in zip(path.split('/'), breadcrumbs.text.split('\n')):
        assert dir1 == dir2, '{:s} == {:s}'.format(dir1, dir2)


@when(parsers.parse('user of {browser_id} changes current working directory '
                    'to {path} using breadcrumbs'))
@then(parsers.parse('user of {browser_id} changes current working directory '
                    'to {path} using breadcrumbs'))
def change_cwd_using_breadcrumbs(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    breadcrumbs = driver.find_elements_by_css_selector('#main-content '
                                                       '.secondary-top-bar '
                                                       '.file-breadcrumbs-list '
                                                       '.file-breadcrumbs-item '
                                                       'a')
    dir1, dir2 = None, None
    for dir1, dir2 in zip(path.split('/'), breadcrumbs):
        assert dir1 == dir2.text, '{} == {}'.format(dir1, dir2.text)
    dir2.click()
