"""Steps for features of Oneprovider shares.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
import os
import time

from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND, MAX_REFRESH_COUNT
from tests.gui.utils.generic import upload_file_path
from pytest_bdd import when, then, parsers
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains

from tests.gui.utils.generic import refresh_and_call
from pytest_selenium_multi.pytest_selenium_multi import select_browser


import tests.gui.utils.file_system as fs
from tests.utils.acceptance_utils import list_parser


@when(parsers.parse('user of {browser_id} sees that new share named '
                    '"{share_name}" was created from '
                    '{item_type} "{item_name}"'))
@then(parsers.parse('user of {browser_id} sees that new share named '
                    '"{share_name}" was created from '
                    '{item_type} "{item_name}"'))
def create_share(browser_id, share_name, item_type, item_name, tmp_memory):
    assert item_type in ('file', 'directory')
    cur_dir = tmp_memory[browser_id]['website']['current_dir']
    fs.mkshare(browser_id, share_name, cur_dir.files[item_name], tmp_memory)


@when(parsers.parse('user of {browser_id} does not see any share info'))
@then(parsers.parse('user of {browser_id} does not see any share info'))
def check_that_there_is_no_share_info(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    assert not driver.find_elements_by_css_selector('#content-scroll '
                                                    '.share-info-head')


@when(parsers.parse('user of {browser_id} does sees valid share info '
                    'for "{share_name}"'))
@then(parsers.parse('user of {browser_id} does sees valid share info '
                    'for "{share_name}"'))
def check_share_info(selenium, browser_id, share_name, tmp_memory):
    driver = select_browser(selenium, browser_id)
    share = tmp_memory[browser_id]['shares'][share_name]

    name, breadcrumbs = Wait(driver, WAIT_FRONTEND).until(
        lambda d: d.find_elements_by_css_selector('#content-scroll '
                                                  '.share-info-head '
                                                  '.share-name, '
                                                  '#content-scroll '
                                                  '.share-info-head '
                                                  '.file-breadcrumbs-list'),
        message='waiting for share {:s} info to appear'.format(share_name)
    )
    assert name.text == share_name
    displayed_path = breadcrumbs.text.split()
    for dir1, dir2 in zip(displayed_path, fs.get_path(share.shared)):
        assert dir1 == dir2
