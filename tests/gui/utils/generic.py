"""Generic GUI testing utils - mainly helpers and extensions for Selenium.
"""


import re
import os
import itertools
from time import sleep, time
from functools import wraps
from contextlib import contextmanager

from tests import gui
from tests.gui.conftest import SELENIUM_IMPLICIT_WAIT, WAIT_REFRESH, WAIT_FRONTEND
from selenium.webdriver.support.wait import WebDriverWait as Wait
from selenium.common.exceptions import TimeoutException, NoSuchElementException


__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


# RE_URL regexp is matched as shown below:
#
# https://172.18.0.8/#/onedata/data/small_space/g2gDZAAEZ3VpZG0AAAAkZzJnQ
# \       \        /   \     / \  / \         /                        /
#  \        domain      \   /   tab  \___id__/                        /
#   \            /      access                                       /
#    \_base_url_/         \_________________method__________________/

RE_URL = re.compile(r'(?P<base_url>https?://(?P<domain>.*?))'
                    r'(/#)?(?P<method>/(?P<access>[^/]*)/(?P<tab>[^/]*)'
                    r'(/(?P<id>[^/]*).*)?)')


def parse_url(url):
    return RE_URL.match(url)


def go_to_relative_url(selenium, relative_url):
    new_url = RE_URL.match(selenium.current_url).group('base_url') + relative_url
    selenium.get(new_url)


def parse_seq(seq):
    return [el.strip().strip('"') for el in seq.strip("[]").split(',') if el != ""]


# A draft - not tested yet, but can be helpful in the future
def change_implicit_wait(driver, fun, wait_time):
    """This will invoke fun(driver), chaining implicitly_wait for time of execution
    WARNING: this will change implicit_wait time on global selenium object!
    Returns the result of fun invocation
    """
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


def refresh_and_call(browser, callback, *args, **kwargs):
    """Refresh browser and keep calling callback with given args
    until achieve expected result or timeout.
    """
    browser.refresh()
    try:
        result = Wait(browser, WAIT_REFRESH).until(
            lambda s: callback(s, *args, **kwargs)
        )
    except TimeoutException:
        return None
    else:
        return result


def enter_text(input_box, text):
    input_box.clear()
    input_box.send_keys(text)
    return True if input_box.get_attribute('value') == text else False


@contextmanager
def implicit_wait(driver, timeout, prev_timeout):
    driver.implicitly_wait(timeout)
    try:
        yield
    finally:
        driver.implicitly_wait(prev_timeout)


def repeat_failed(attempts=10, interval=0.01, timeout=-1, poll_frequency=0.5,
                  exceptions=Exception):

    def wrapper(function):
        @wraps(function)
        def repeat_until(*args, **kwargs):
            if timeout > 0:
                limit = time() + timeout
                sleep_time = poll_frequency
                i = time()
            else:
                limit = attempts
                sleep_time = interval
                i = 0

            while i < limit:
                try:
                    result = function(*args, **kwargs)
                except exceptions:
                    sleep(sleep_time)
                    i = time() if timeout > 0 else i+1
                    continue
                else:
                    return result
            return function(*args, **kwargs)
        return repeat_until
    return wrapper


def iter_ahead(iterable):
    read_ahead = iter(iterable)
    next(read_ahead)
    for item, next_item in itertools.izip(iterable, read_ahead):
        yield item, next_item


def find_web_elem(web_elem, css_sel, err_msg):
    try:
        item = web_elem.find_element_by_css_selector(css_sel)
    except NoSuchElementException:
        raise RuntimeError(err_msg)
    else:
        return item


def find_web_elem_with_text(web_elem, css_sel, text, err_msg):
    items = web_elem.find_elements_by_css_selector(css_sel)
    for item in items:
        if item.text.lower() == text:
            return item
    else:
        raise RuntimeError(err_msg)


@contextmanager
def suppress(*exceptions):
    try:
        yield
    except exceptions:
        pass
