"""Steps for features of Onezone login page.
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND
from tests.gui.utils.generic import upload_file_path, find_element
from pytest_bdd import when, then, parsers, given
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from pytest import fixture


@fixture
def get_provider_name(provider_name):
    return provider_name


@given(parsers.parse('existing provider "{provider_name}" supporting our space named "{space_name}"'))
def existing_provider_supporting_space(provider_name, space_name):
    return provider_name


@given(parsers.parse('existing file name "{file_name}"'))
def existing_file(file_name):
    return file_name


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


@when('user clicks "Create file" button from top menu bar')
def click_create_new_file_button(selenium):
    new_file_button = Wait(selenium, WAIT_BACKEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, 'ul.navbar-nav a#create-file-tool'))
    )
    new_file_button.click()


@when('user should see, that input box for file name is active')
def wait_input_box_for_file_name_is_active(selenium):

    def _is_active(selenium):
        elem = selenium.find_elements_by_css_selector(
            '#create-file-modal .ember-view input.form-control')
        if elem:
            elem = elem[0]
            return elem == selenium.switch_to.active_element
        else:
            return False

    Wait(selenium, WAIT_FRONTEND).until(_is_active)


@then('user should not see input box for file name')
def check_if_input_box_for_file_name_disappeared(selenium):
    Wait(selenium, WAIT_FRONTEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR, '#create-file-modal .ember-view input.form-control'))
    )


@then(parsers.parse('user should see new file named "{file_name}" in files list'))
def check_existence_of_new_file(selenium, file_name):
    added_file = find_element(selenium, 'table.table td.file-list-col-file', file_name)
    Wait(selenium, WAIT_FRONTEND).until(lambda s: added_file is not None)


@when('user clicks "Create directory" button from top menu bar')
def click_create_new_directory_button(selenium):
    new_directory_button = Wait(selenium, WAIT_BACKEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, 'ul.navbar-nav a#create-dir-tool'))
    )
    new_directory_button.click()


@when('user should see, that input box for directory name is active')
def wait_input_box_for_directory_name_is_active(selenium):

    def _is_active(selenium):
        elem = selenium.find_elements_by_css_selector(
            '#create-dir-modal .ember-view input.form-control')
        if elem:
            elem = elem[0]
            return elem == selenium.switch_to.active_element
        else:
            return False

    Wait(selenium, WAIT_FRONTEND).until(_is_active)


@then('user should not see input box for directory name')
def check_if_input_box_for_directory_name_disappeared(selenium):
    Wait(selenium, WAIT_FRONTEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR, '#create-dir-modal .ember-view input.form-control'))
    )


@then(parsers.parse('user should see new directory named "{dir_name}" in files list'))
def check_existence_of_new_directory(selenium, dir_name):
    added_dir = find_element(selenium, 'table.table td.file-list-col-file', dir_name)
    Wait(selenium, WAIT_FRONTEND).until(lambda s: added_dir is not None)


@when(parsers.parse('user selects "{file_name}" from files list'))
def select_file(selenium, file_name):

    def find_file(s):
        files = s.find_elements_by_css_selector('.files-list table.files-table td.file-list-col-file')
        for elem in files:
            if elem.text == file_name:
                return elem
        return None

    Wait(selenium, WAIT_FRONTEND).until(find_file).click()


@when('user clicks "Remove element" button from top menu bar')
def click_remove_button(selenium):
    remove_button = Wait(selenium, WAIT_FRONTEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, '#navbar-collapse ul.nav a#remove-file-tool'))
    )
    remove_button.click()


@when('user clicks "OK" button')
def click_ok_button(selenium):
    ok_button = Wait(selenium, WAIT_FRONTEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, '#remove-files-modal button.btn-primary'))
    )
    ok_button.click()


@then(parsers.parse('user should not see file named "{file_name}" in files list'))
def check_absence_deleted_file(selenium, file_name):
    deleted_file = find_element(selenium, 'table.table td.file-list-col-file', file_name)
    assert deleted_file is None


@when('user clicks "Show file distribution" button from top menu bar')
def click_show_file_distribution_button(selenium):

    show_file_distribution_button = Wait(selenium, WAIT_BACKEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, '#navbar-collapse ul.nav a#file-chunks-tool'))
    )
    show_file_distribution_button.click()


@then(parsers.parse('user should see provider name "{provider_name}" in providers column'))
def check_if_provider_name_in_table(selenium, provider_name):

    def _find_provider(s):
        providers = s.find_elements_by_css_selector(
            '#file-chunks-modal .container-fluid table.file-blocks-table td.provider-name')
        for elem in providers:
            if elem.text == provider_name:
                return elem
        return None

    Wait(selenium, WAIT_FRONTEND).until(_find_provider)


###############################################################################################################
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