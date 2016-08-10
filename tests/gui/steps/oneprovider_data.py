"""Steps for features of Onezone login page.
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND
from tests.gui.utils.generic import upload_file_path
from pytest_bdd import when, then, parsers
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By



@when('user should see, that new file name input box is active')
def wait_new_file_name_input_box_is_active(selenium):

    def _is_active(selenium):
        elem = selenium.find_elements_by_css_selector(
            '#create-file-modal .ember-view input.form-control')
        if elem:
            elem = elem[0]
            return elem == selenium.switch_to.active_element
        else:
            return False

    Wait(selenium, WAIT_FRONTEND).until(_is_active)


@when('user should see, that new directory name input box is active')
def wait_new_directory_name_input_box_is_active(selenium):

    def _is_active(selenium):
        elem = selenium.find_elements_by_css_selector(
            '#create-dir-modal .ember-view input.form-control')
        if elem:
            elem = elem[0]
            return elem == selenium.switch_to.active_element
        else:
            return False

    Wait(selenium, WAIT_FRONTEND).until(_is_active)


@when('user clicks "Create file" button')
def click_create_new_file_button(selenium):
    new_file_button = Wait(selenium, WAIT_BACKEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, 'ul.navbar-nav a#create-file-tool'))
    )
    new_file_button.click()


@when('user clicks "Create directory" button')
def click_create_new_directory_button(selenium):
    new_directory_button = Wait(selenium, WAIT_BACKEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, 'ul.navbar-nav a#create-dir-tool'))
    )
    new_directory_button.click()


@then(parsers.parse('user should see "{file_name}" file'))
def check_new_file(selenium, file_name):

    def find_added_file(s):
        files = s.find_elements_by_css_selector('table.table td.file-list-col-file')
        for elem in files:
            if elem.text == file_name:
                return elem
        return None

    Wait(selenium, WAIT_FRONTEND).until(find_added_file)


@then(parsers.parse('user should see "{dir_name}" directory'))
def check_new_directory(selenium, dir_name):

    def find_added_directory(s):
        files = s.find_elements_by_css_selector('table.table td.file-list-col-file')
        for elem in files:
            if elem.text == dir_name:
                return elem
        return None

    Wait(selenium, WAIT_FRONTEND).until(find_added_directory)


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

    def file_browser_ready(driver):
        files_table = driver.find_element_by_css_selector('.files-table')
        return not re.match(r'.*is-loading.*', files_table.get_attribute('class'))

    Wait(selenium, WAIT_BACKEND).until(file_browser_ready)


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


# @when(parsers.parse('The upload of file "{file_name}" fails'))
# @then(parsers.parse('The upload of file "{file_name}" should fail'))
# def upload_fails(selenium, file_name):
#     Wait(selenium, 2*WAIT_BACKEND).until(
#         lambda s: notify_visible_with_text(s, 'error', re.compile(r'.*' + file_name + r'.*' + 'failed' + r'.*'))
#     )
#
#
# @then(parsers.parse('The upload of file "{file_name}" should succeed'))
# def upload_succeeds(selenium, file_name):
#     Wait(selenium, 2*WAIT_BACKEND).until(
#         lambda s: notify_visible_with_text(s, 'info', re.compile(r'.*' + file_name + r'.*' + 'successfully' + r'.*'))
#     )