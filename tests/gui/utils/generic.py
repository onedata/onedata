"""Generic GUI testing utils - mainly helpers and extensions for Selenium.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


import re
import os
from tests import gui

from tests.gui.conftest import SELENIUM_IMPLICIT_WAIT

RE_URL = re.compile(r'(?P<base_url>https?://(?P<domain>.*?))(/#)?(?P<method>/.*)')
RE_DATA_URL = re.compile(r'(?P<lang>/.*)?/data/(?P<space>.*)/(?P<dir>.*)')

def parse_url(url):
    return RE_URL.match(url)


def go_to_relative_url(selenium, relative_url):
    new_url = RE_URL.match(selenium.current_url).group('base_url') + relative_url
    selenium.get(new_url)


# TODO: not tested yet
def change_implicit_wait(driver, fun, wait_time):
    """This will invoke fun(driver), chaning implicitly_wait for time of execution
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


# TODO: not tested yet
def is_element_present_by_css(driver, css_selector):
    return change_implicit_wait(
        driver,
        driver.find_elements_by_css_selector(css_selector),
        0) > 0


# TODO: a oneprovider helper - move to another module
def current_dir(driver):
    return RE_DATA_URL.match(
        parse_url(driver.current_url).group('method')
    ).group('dir')


def upload_file_path(file_name):
    """Resolve an absolute path for file with name file_name stored in upload_files dir
    """
    return os.path.join(
        os.path.dirname(os.path.abspath(gui.__file__)),
        'upload_files',
        file_name
    )