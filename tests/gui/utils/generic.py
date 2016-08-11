"""Generic GUI testing utils - mainly helpers and extensions for Selenium.
"""
from selenium.common.exceptions import NoSuchElementException

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
import os
from tests import gui
from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from tests.gui.conftest import SELENIUM_IMPLICIT_WAIT
from selenium.webdriver.common.by import By


RE_URL = re.compile(r'(?P<base_url>https?://(?P<domain>.*?))(/#)?(?P<method>/.*)')


def parse_url(url):
    return RE_URL.match(url)


def go_to_relative_url(selenium, relative_url):
    new_url = RE_URL.match(selenium.current_url).group('base_url') + relative_url
    selenium.get(new_url)


# A draft - not tested yet, but can be helpful in the future
def change_implicit_wait(driver, fun, wait_time):
    """This will invoke fun(driver), chaining implicitly_wait for time of execution
    WARNING: this will change implicit_wait time on global selenium object!
    Returns the result of fun invocation
    """
    result = None
    try:
        driver.implicitly_wait(wait_time)
        result = fun(driver)
    finally:
        driver.implicitly_wait(SELENIUM_IMPLICIT_WAIT)

    return result


def upload_file_path(file_name):
    """Resolve an absolute path for file with name file_name stored in upload_files dir
    """
    return os.path.join(
        os.path.dirname(os.path.abspath(gui.__file__)),
        'upload_files',
        file_name
    )


def find_element(selenium, selector, text):
    """finds element on site by css selector and element's text"""
    elements_list = selenium.find_elements_by_css_selector(selector)
    for elem in elements_list:
        if elem.text == text:
            return elem
    return None


def get_text_from_input_box(selenium):
    input_box = Wait(selenium, WAIT_FRONTEND).until(
        EC.visibility_of_element_located((By.CSS_SELECTOR,
                                          '.input-with-button input#invite-form-token-userJoinSpace-field'))
    )
    text = input_box.get_attribute('value')
    return text
