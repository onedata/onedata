"""Generic GUI testing utils - mainly helpers and extensions for Selenium.
"""


import re
import os
from time import sleep, time
from itertools import islice, izip
from contextlib import contextmanager

from decorator import decorator

from tests import gui
from tests.gui.conftest import SELENIUM_IMPLICIT_WAIT, WAIT_REFRESH
from selenium.webdriver.support.wait import WebDriverWait as Wait
from selenium.common.exceptions import TimeoutException, NoSuchElementException
from selenium.webdriver.common.action_chains import ActionChains


__author__ = "Jakub Liput, Bartosz Walkowicz"
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


def parse_seq(seq, pattern=None, default=str):
    if pattern is not None:
        return [default(el.group()) for el in re.finditer(pattern, seq)]
    else:
        return [default(el.strip().strip('"'))
                for el in seq.strip("[]").split(',') if el != ""]


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


def repeat_failed(attempts=None, timeout=None, interval=0.1,
                  exceptions=(Exception,)):
    """Returns wrapper on function, which keeps calling it until timeout or
    for attempts times in case of failure (exception).

    :param attempts: maximum num of attempts
    :type attempts: int | None
    :param interval: time between subsequent calls
    :type interval: float
    :param timeout: time limit of now when to stop repeating fun
    :type timeout: float | None
    :param exceptions: in case of which consider failure of call
    :type exceptions: list[Exception]
    :return: wrapper decorator
    """
    if not isinstance(attempts, int) and not isinstance(timeout, float):
        raise ValueError('none of attempts or timeout set')
    elif isinstance(attempts, int) and isinstance(timeout, float):
        raise ValueError('both attempts and timeout set')

    @decorator
    def wrapper(fun, *args, **kwargs):
        now = time()
        limit, i = (now + timeout, now) if timeout else (attempts, 0)

        while i < limit:
            try:
                result = fun(*args, **kwargs)
            except exceptions:
                sleep(interval)
                i = time() if timeout else i+1
                continue
            else:
                return result
        return fun(*args, **kwargs)

    return wrapper


def iter_ahead(iterable):
    read_ahead = iter(iterable)
    next(read_ahead)
    for item, next_item in izip(iterable, read_ahead):
        yield item, next_item


def find_web_elem(web_elem_root, css_sel, err_msg):
    try:
        item = web_elem_root.find_element_by_css_selector(css_sel)
    except NoSuchElementException:
        with suppress(TypeError):
            err_msg = err_msg()
        raise RuntimeError(err_msg)
    else:
        return item


def find_web_elem_with_text(web_elem_root, css_sel, text, err_msg):
    items = web_elem_root.find_elements_by_css_selector(css_sel)
    for item in items:
        if item.text.lower() == text.lower():
            return item
    else:
        with suppress(TypeError):
            err_msg = err_msg()
        raise RuntimeError(err_msg)


def click_on_web_elem(driver, web_elem, err_msg, delay=True):
    disabled = 'disabled' in web_elem.get_attribute('class')
    if web_elem.is_enabled() and web_elem.is_displayed() and not disabled:
        # TODO make optional sleep and localize only those tests that need it or find better alternative
        # currently checking if elem is enabled not always work (probably after striping disabled from web elem
        # elem is not immediately clickable)
        if delay:
            sleep(delay if isinstance(delay, float) else 0.25)
        action = ActionChains(driver)
        action.move_to_element(web_elem).click_and_hold(web_elem).release(web_elem)
        action.perform()
    else:
        with suppress(TypeError):
            err_msg = err_msg()
        raise RuntimeError(err_msg)


@contextmanager
def suppress(*exceptions):
    try:
        yield
    except exceptions:
        pass


@contextmanager
def rm_css_cls(driver, web_elem, css_cls):
    driver.execute_script("$(arguments[0]).removeClass('{}')".format(css_cls),
                          web_elem)
    yield web_elem
    driver.execute_script("$(arguments[0]).addClass('{}')".format(css_cls),
                          web_elem)


def nth(seq, idx):
    return next(islice(seq, idx, None), None)
