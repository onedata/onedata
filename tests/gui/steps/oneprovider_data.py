"""Steps for features of Onezone login page.
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND
from tests.gui.utils.generic import upload_file_path
from tests.gui.steps.common import find_element_by_css_selector_and_text,\
    select_button_from_buttons_by_name
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


@then(parsers.parse('user should see new directory named "{elem_name}" in files list'))
@then(parsers.parse('user should see new file named "{elem_name}" in files list'))
def op_check_if_new_element_appeared(selenium, elem_name):
    Wait(selenium, WAIT_FRONTEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR, 'table.is-loading'))
    )
    new_elem = find_element_by_css_selector_and_text('table.table td.file-list-col-file',
                                                     elem_name)
    Wait(selenium, WAIT_FRONTEND).until(new_elem)


@when(parsers.parse('user selects "{elem_name}" from files list'))
def op_select_elem(selenium, elem_name):
    Wait(selenium, WAIT_FRONTEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR, 'table.is-loading'))
    )
    elem = select_button_from_buttons_by_name(elem_name,
                                              '.files-list table.files-table td.file-list-col-file')
    Wait(selenium, WAIT_FRONTEND).until(elem).click()


@then(parsers.parse('user should not see directory named "{elem_name}" in files list'))
@then(parsers.parse('user should not see file named "{elem_name}" in files list'))
def check_absence_deleted_element(selenium, elem_name):

    def _try_find_deleted_element(s):
        elems = s.find_elements_by_css_selector('table.table td.file-list-col-file')
        for elem in elems:
            if elem.text == elem_name:
                return elem
        return None

    Wait(selenium, WAIT_FRONTEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR, 'table.is-loading'))
    )
    assert _try_find_deleted_element(selenium) is None


@then(parsers.parse('user should see modal with provider\'s name "{provider_name}" in providers column'))
def op_check_if_provider_name_is_in_tab(selenium, provider_name):

    def _find_provider(s):
        providers = s.find_elements_by_css_selector(
            '#file-chunks-modal .container-fluid table.file-blocks-table td.provider-name')
        for elem in providers:
            if elem.text == provider_name:
                return elem
        return None

    Wait(selenium, WAIT_FRONTEND).until(_find_provider)


# TODO ask if we need this functions
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

