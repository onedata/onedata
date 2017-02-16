# coding=utf-8
"""Steps for features of Onezone login page.
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import time

from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND
from tests.gui.utils.generic import upload_file_path, repeat
from pytest_bdd import when, then, parsers, given
from pytest_selenium_multi.pytest_selenium_multi import select_browser

from selenium.webdriver.support.ui import WebDriverWait as Wait


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
    Wait(driver, WAIT_BACKEND*3).until_not(
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
        raise RuntimeError('directory {} does not exist'.format(str(directory)))


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


@then(parsers.parse('user of {browser_id} sees that chunk bar for provider '
                    'named "{provider}" is entirely filled'))
@then(parsers.parse('user of {browser_id} sees that chunk bar for provider '
                    'named "{provider}" is entirely filled'))
@repeat(attempts=WAIT_BACKEND, timeout=True)
def assert_provider_chunk_in_file_distribution_filled(selenium, browser_id,
                                                      provider):
    driver = select_browser(selenium, browser_id)
    css_sel = '#file-chunks-modal table.file-blocks-table td.provider-name, ' \
              '#file-chunks-modal table.file-blocks-table td.chunks canvas'
    name, canvas = driver.find_elements_by_css_selector(css_sel)
    assert name.text == provider, \
        'provider name displayed {} instead of expected {}'.format(name.text,
                                                                   provider)
    assert driver.execute_script(is_canvas_filled, canvas), \
        'canvas not filled entirely for provider named {}'.format(provider)


is_canvas_filled = """
function is_canvas_filled(cvs){
    var ctx = cvs.getContext("2d");
    var fillColorHex = ctx.fillStyle;
    var fillColor = [parseInt(fillColorHex.slice(1, 3), 16),
                     parseInt(fillColorHex.slice(3, 5), 16),
                     parseInt(fillColorHex.slice(5, 7), 16)];
    img = ctx.getImageData(0, 0, cvs.width, cvs.height);
    pix = img.data;
    for(var i = 0; i < pix.length; i += 4)
        if(pix[i] != fillColor[0] || pix[i+1] != fillColor[1] || pix[i+2] != fillColor[2])
            return false;

    return true;
}

return is_canvas_filled(arguments[0]);
"""
