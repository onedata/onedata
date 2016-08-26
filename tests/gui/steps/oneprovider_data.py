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
from tests.gui.steps.common import find_element_by_css_selector_and_text,\
    select_button_from_buttons_by_name
from pytest_bdd import when, then, parsers, given
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains

from pytest import fixture

from ..utils.inspect import selector
from ..utils.generic import find_item_with_given_properties


@given(parsers.parse('there is no file named "{file_list_elem}" in files list'))
@given(parsers.parse('there is no directory named "{file_list_elem}" in files list'))
def check_if_element_not_exist(selenium, file_list_elem):

    def _find_file(s):
        files = s.find_elements_by_css_selector('.files-list td')
        for f in files:
            if f.text == file_list_elem:
                return f
        return None

    Wait(selenium, WAIT_FRONTEND).until_not(_find_file)


@fixture
def supporting_provider():
    return dict()


def _check_for_item_in_files_list(selenium, name):
    list_items = selenium.find_elements_by_css_selector('.files-list td')
    for item in list_items:
        if item.text == name:
            return True
    return False


@given(parsers.parse('there is a "{file_name}" file on the files list'))
def existing_file(selenium, file_name):
    Wait(selenium, WAIT_FRONTEND).until(
        lambda s: _check_for_item_in_files_list(selenium, file_name)
    )


@when(parsers.re(r'user uses spaces select to change data space to "(?P<space_name>.+)"'))
def change_space(selenium, space_name):
    # HACK: because Firefox driver have buggy EC.element_to_be_clickable,
    # we wait for loader to disappear
    Wait(selenium, WAIT_FRONTEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR, '.common-loader-spinner'))
    )
    Wait(selenium, WAIT_FRONTEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, '.data-spaces-select a[data-toggle=dropdown]'))
    ).click()
    spaces = selenium.find_elements_by_css_selector('.data-spaces-select .dropdown-menu a')

    def space_by_name(_):
        named_spaces = [s for s in spaces if re.match(space_name, s.text.strip(), re.I)]
        if len(named_spaces) > 0 and named_spaces[0].is_enabled():
            return named_spaces[0]
        else:
            return None

    Wait(selenium, WAIT_FRONTEND).until(space_by_name).click()


@when(parsers.parse('user uses upload button in toolbar to upload file "{file_name}" to current dir'))
def upload_file_to_current_dir(selenium, file_name):
    """This interaction is very hacky, because uploading files with Selenium
    needs to use input element, but we do not use it directly in frontend.
    So we unhide an input element for a while and pass a local file path to it.
    """
    # HACK: for Firefox driver - because we cannot interact with hidden elements
    selenium.execute_script("$('input#toolbar-file-browse').removeClass('hidden')")
    selenium.find_element_by_css_selector('input#toolbar-file-browse').send_keys(
        upload_file_path(file_name)
    )
    selenium.execute_script("$('input#toolbar-file-browse').addClass('hidden')")

    def file_browser_ready(driver):
        files_table = driver.find_element_by_css_selector('.files-table')
        return not re.match(r'.*is-loading.*', files_table.get_attribute('class'))

    Wait(selenium, WAIT_BACKEND).until(file_browser_ready)


@then(parsers.parse('user clicks the button from top menu bar with tooltip "{tooltip_name}"'))
@when(parsers.parse('user clicks the button from top menu bar with tooltip "{tooltip_name}"'))
def op_click_tooltip_from_top_menu_bar(selenium, tooltip_name):

    def _find_tooltip_with_given_name(s):
        tooltips = s.find_elements_by_css_selector('ul.toolbar-group a')
        for tooltip in tooltips:
            if tooltip.get_attribute('data-original-title') == tooltip_name:
                return tooltip
        return None

    tooltip = Wait(selenium, WAIT_BACKEND).until(_find_tooltip_with_given_name)
    tooltip.click()


@when(parsers.parse('user sees new file named "{file_list_element}" in files list'))
@then(parsers.parse('user sees new directory named "{file_list_element}" in files list'))
@then(parsers.parse('user sees new file named "{file_list_element}" in files list'))
def op_check_if_new_element_appeared(selenium, file_list_element):
    new_file_list_elem = find_element_by_css_selector_and_text('table.table td.file-list-col-file',
                                                               file_list_element)
    Wait(selenium, WAIT_FRONTEND).until(new_file_list_elem)


@then(parsers.parse('user selects "{file_list_element}" from files list'))
@when(parsers.parse('user selects "{file_list_element}" from files list'))
def op_select_elem(selenium, file_list_element):
    file_list_elem_to_select = select_button_from_buttons_by_name(file_list_element,
                                                                  '.files-list table.files-table td.file-list-col-file')
    Wait(selenium, WAIT_FRONTEND).until(file_list_elem_to_select).click()


@then(parsers.parse('user should not see directory named "{file_list_element}" in files list'))
@then(parsers.parse('user should not see file named "{file_list_element}" in files list'))
def check_absence_deleted_element(selenium, file_list_element):

    def _try_find_deleted_element(s):
        elems = s.find_elements_by_css_selector('table.table td.file-list-col-file')
        for elem in elems:
            if elem.text == file_list_element:
                return elem
        return None

    assert _try_find_deleted_element(selenium) is None


@then(parsers.parse('user sees modal with name of provider supporting '
                    'space in providers column'))
def op_check_if_provider_name_is_in_tab(selenium, supporting_provider):

    def _find_provider(s):
        providers = s.find_elements_by_css_selector(
            '#file-chunks-modal .container-fluid table.file-blocks-table td.provider-name')
        for elem in providers:
            if elem.text == supporting_provider['name']:
                return elem
        return None

    Wait(selenium, WAIT_FRONTEND).until(_find_provider)


@then(parsers.parse('user double clicks on file "{file_item}" from files list'))
@when(parsers.parse('user double clicks on file "{file_item}" from files list'))
def op_select_file_from_file_list(selenium, file_item):
    check_properties = selector(selenium, text=file_item,
                                check_visibility=True)
    css_path = '.files-list table.files-table td.file-list-col-file'

    list_item = Wait(selenium, WAIT_FRONTEND).until(
        lambda s: find_item_with_given_properties(s, css_path,
                                                  check_properties)
    )
    ActionChains(selenium).double_click(list_item).perform()


@then(parsers.parse('user sees that downloaded file "{file_name}" '
                    'contains "{content}"'))
def check_if_downloaded_file_contains_given_content(tmpdir, file_name,
                                                    content):
    tmpdir_path = str(tmpdir)
    file_path = os.path.join(tmpdir_path, file_name)

    # sleep waiting for file to finish downloading
    for sleep_time in range(10):
        if not os.listdir(tmpdir_path):
            time.sleep(sleep_time)

    with open(file_path, 'r') as f:
        file_content = ''.join(f.readlines())
        assert content == file_content
