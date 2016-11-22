"""Steps for features of Onezone login page.
"""
from selenium.common.exceptions import StaleElementReferenceException

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
import time

from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND
from tests.gui.utils.generic import upload_file_path
from tests.gui.utils.oneprovider_gui import assert_breadcrumbs_correctness, \
    chdir_using_breadcrumbs

from pytest_bdd import when, then, parsers, given
from pytest_selenium_multi.pytest_selenium_multi import select_browser

from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By


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
                    'upload files from local directory "{dir_path}" to remote '
                    'current dir'))
def upload_files_to_cwd(selenium, browser_id, dir_path, tmpdir):
    driver = select_browser(selenium, browser_id)
    directory = tmpdir.join(browser_id, *dir_path.split('/'))
    if directory.isdir():
        _upload_files_to_cwd(driver, '\n'.join(str(item) for item
                                               in directory.listdir()
                                               if item.isfile()))
    else:
        raise ValueError('directory {} does not exist'.format(str(directory)))


# TODO currently every browser in test download to that same dir,
# repair it by changing capabilities in pytest selenium mult
@when(parsers.parse('user of {browser_id} sees that content of downloaded '
                    'file "{file_name}" is equal to: "{content}"'))
@then(parsers.parse('user of {browser_id} sees that content of downloaded '
                    'file "{file_name}" is equal to: "{content}"'))
def has_downloaded_file_content(selenium, tmpdir, file_name,
                                content, browser_id):
    driver = select_browser(selenium, browser_id)
    downloaded_file = tmpdir.join(file_name)

    # sleep waiting for file to finish downloading
    exist = False
    sleep_time = 5
    for _ in range(10):
        if not exist:
            time.sleep(sleep_time)
        else:
            break
        exist = downloaded_file.isfile()

    assert exist, 'file {} has not been downloaded'.format(file_name)

    def _check_file_content():
        with downloaded_file.open() as f:
            file_content = ''.join(f.readlines())
            return content == file_content

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


def _is_space_tree_root(driver, is_home, space_name):
    space_select = driver.find_element_by_css_selector('nav.secondary-sidebar '
                                                       '.data-spaces-select')
    icon, label = space_select.find_elements_by_css_selector('.dropdown > a '
                                                             '.item-icon'
                                                             ':not(.select-arrow) '
                                                             '.oneicon, '
                                                             '.dropdown > a '
                                                             '.item-label')
    displayed_name = label.text
    assert displayed_name == space_name, 'current directory tree is displayed ' \
                                         'for "{}" instead of "{}"'.format(displayed_name,
                                                                           space_name)
    if is_home:
        assert 'oneicon-space-home' in icon.get_attribute('class'), \
            'space {} is not home space'.format(displayed_name)


@given(parsers.re('user of (?P<browser_id>.+?) seen that displayed directory '
                  'tree in sidebar panel belonged to (?P<is_home>(home )?)space '
                  'named "(?P<space_name>.+?)'))
def g_is_space_tree_root(selenium, browser_id, is_home, space_name):
    driver = select_browser(selenium, browser_id)
    _is_space_tree_root(driver, True if is_home else False, space_name)


@when(parsers.re('user of (?P<browser_id>.+?) sees that displayed directory '
                 'tree in sidebar panel belongs to (?P<is_home>(home )?)space '
                 'named "(?P<space_name>.+?)"'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that displayed directory '
                 'tree in sidebar panel belongs to (?P<is_home>(home )?)space '
                 'named "(?P<space_name>.+?)"'))
def wt_is_space_tree_root(selenium, browser_id, is_home, space_name):
    driver = select_browser(selenium, browser_id)
    _is_space_tree_root(driver, True if is_home else False, space_name)


# TODO implement better checking dir tree
@when(parsers.parse('user of {browser_id} sees that current working directory '
                    'displayed in breadcrumbs is {path}'))
@then(parsers.parse('user of {browser_id} sees that current working directory '
                    'displayed in breadcrumbs is {path}'))
def is_displayed_path_correct(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    breadcrumbs = driver.find_element_by_css_selector('#main-content '
                                                      '.secondary-top-bar '
                                                      '.file-breadcrumbs-list')
    assert_breadcrumbs_correctness(path, breadcrumbs)


@when(parsers.parse('user of {browser_id} changes current working directory '
                    'to {path} using breadcrumbs'))
@then(parsers.parse('user of {browser_id} changes current working directory '
                    'to {path} using breadcrumbs'))
def change_cwd_using_breadcrumbs(selenium, browser_id, path):
    driver = select_browser(selenium, browser_id)
    # HACK: a workaround for fast multiple breadcrumbs re-computations leading to
    # quick DOM changes between find elements and chdir_using_breadcrumbs
    tries = 10
    while tries > 0:
        breadcrumbs = driver.find_elements_by_css_selector('#main-content '
                                                           '.secondary-top-bar '
                                                           '.file-breadcrumbs-list '
                                                           '.file-breadcrumbs-item '
                                                           'a')
        try:
            chdir_using_breadcrumbs(path, breadcrumbs)
        except StaleElementReferenceException:
            tries -= 1
            if tries <= 0:
                raise RuntimeError(('A StaleElementReferenceException has been thrown %s times. ' % tries) +
                                   'Breadcrumbs was probably rendered multiple times between find_elements and elements usage.')
        else:
            tries = 0
