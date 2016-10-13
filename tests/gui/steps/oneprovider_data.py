"""Steps for features of Onezone login page.
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
import os
import time

from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND
from tests.gui.utils.generic import upload_file_path
from pytest_bdd import when, then, parsers
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


@when(parsers.parse('user of {browser_id} sees that content of downloaded '
                    'file "{file_name}" is as: {content}'))
@then(parsers.parse('user of {browser_id} sees that content of downloaded '
                    'file "{file_name}" is as: {content}'))
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


# TODO implement better checking dir tree
@when(parsers.parse('user of {browser_id} sees that current working directory '
                    'displayed in sidebar list is {path}'))
@then(parsers.parse('user of {browser_id} sees that current working directory '
                    'displayed in sidebar list is {path}'))
def is_displayed_path_correct(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    path = path.split('/')
    dir_name = driver.find_element_by_css_selector('.data-files-tree '
                                                   'li.level-{} .active'
                                                   ''.format(len(path) - 1))
    assert dir_name.text == path[-1]
