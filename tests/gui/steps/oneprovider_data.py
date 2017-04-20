"""Steps for features of Onezone login page.
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import time

from tests.gui.conftest import WAIT_BACKEND
from pytest_bdd import when, then, parsers

from selenium.webdriver.support.ui import WebDriverWait as Wait


# TODO currently every browser in test download to that same dir,
# repair it by changing capabilities in pytest selenium mult
@when(parsers.parse('user of {browser_id} sees that content of downloaded '
                    'file "{file_name}" is equal to: "{content}"'))
@then(parsers.parse('user of {browser_id} sees that content of downloaded '
                    'file "{file_name}" is equal to: "{content}"'))
def has_downloaded_file_content(selenium, tmpdir, file_name,
                                content, browser_id):
    driver = selenium[browser_id]
    downloaded_file = tmpdir.join(browser_id, 'download', file_name)

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
