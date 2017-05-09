"""Steps used in creation of browsers
"""

import os
import re
import time
from itertools import cycle

from pytest_bdd import given, parsers
from selenium.webdriver import Firefox, FirefoxProfile
from selenium.webdriver.firefox.firefox_binary import FirefoxBinary
from selenium.webdriver.firefox.options import Options

from tests import gui
from tests.gui.utils.generic import parse_seq
from tests.gui.conftest import SELENIUM_IMPLICIT_WAIT
from tests.gui.utils.generic import redirect_display


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


Firefox.log_types = ['browser']


@given(parsers.parse("user opened {browser_id_list} window"))
@given(parsers.parse("users opened {browser_id_list} browsers' windows"))
def create_instances_of_webdriver(selenium, driver, browser_id_list, tmpdir,
                                  tmp_memory, driver_kwargs, driver_type,
                                  firefox_logging, firefox_path, xvfb,
                                  screen_width, screen_height, displays):

    for browser_id, display in zip(parse_seq(browser_id_list), cycle(xvfb)):
        if browser_id in selenium:
            raise AttributeError('{:s} already in use'.format(browser_id))
        else:
            tmp_memory[browser_id] = {'shares': {},
                                      'spaces': {},
                                      'groups': {},
                                      'mailbox': {},
                                      'oz': {},
                                      'window': {'modal': None}}

            with redirect_display(display):
                temp_dir = str(tmpdir)
                download_dir = os.path.join(temp_dir, browser_id, 'download')

                if driver_type.lower() == 'chrome':
                    options = driver_kwargs['desired_capabilities']['chromeOptions']
                    prefs = {"download.default_directory": download_dir}
                    options['prefs'].update(prefs)

                elif driver_type.lower() == 'firefox':
                    options = Options()
                    profile = FirefoxProfile()
                    log_path = _set_firefox_profile(profile, browser_id,
                                                    temp_dir, firefox_logging)
                    options.profile = profile
                    if firefox_path is not None:
                        options.binary = FirefoxBinary(firefox_path)
                    driver_kwargs['firefox_options'] = options

                browser = driver(driver_kwargs)
                if driver_type.lower() == 'firefox' and firefox_logging:
                    browser.get_log = _firefox_logger(log_path)
                _config_driver(browser, screen_width, screen_height)

            displays[browser_id] = display
            selenium[browser_id] = browser


# TODO: configure different window sizes for responsiveness tests: https://jira.plgrid.pl/jira/browse/VFS-2205
def _config_driver(driver, window_width, window_height):
    driver.implicitly_wait(SELENIUM_IMPLICIT_WAIT)
    driver.set_window_size(window_width, window_height)
    # possible solution to chromedriver cruches: Timed out receiving message from renderer
    driver.set_page_load_timeout(60)
    # currenlty, we rather set window size
    # driver.maximize_window()


def _set_firefox_profile(profile, browser_id, base_dir, logging=False):
    """Set downloading capabilities (dest, popups, ...) and logging"""
    download_dir = os.path.join(base_dir, browser_id, 'download')
    logs_path = os.path.join(base_dir, browser_id, 'logs', 'firefox.log')

    profile.set_preference('browser.download.folderList', 2)
    profile.set_preference('browser.download.manager.showWhenStarting', False)
    profile.set_preference('browser.helperApps.alwaysAsk.force', False)
    profile.set_preference('browser.download.dir', download_dir)
    profile.set_preference('browser.helperApps.neverAsk.saveToDisk',
                           'text/anytext, text/plain, text/html')

    if logging:
        gui_tests_dir = os.path.dirname(os.path.abspath(gui.__file__))
        firebug_path = os.path.join(gui_tests_dir, 'firefox_extensions',
                                    'firebug-2.0.17.xpi')
        console_exp_path = os.path.join(gui_tests_dir, 'firefox_extensions',
                                        'consoleExport-0.5b5.xpi')

        profile.add_extension(firebug_path)
        profile.add_extension(console_exp_path)
        profile.set_preference('extensions.firebug.consoleexport.active',
                               'true')
        profile.set_preference('extensions.firebug.consoleexport.logFilePath',
                               logs_path)
        profile.set_preference('extensions.firebug.framePosition', 'detached')
        profile.set_preference("extensions.firebug.currentVersion", "2.0")
        profile.set_preference("extensions.firebug.console.enableSites", 'true')
        profile.set_preference("extensions.firebug.net.enableSites", 'true')
        profile.set_preference("extensions.firebug.script.enableSites", 'true')
        profile.set_preference("extensions.firebug.allPagesActivation", 'on')

    profile.update_preferences()
    return logs_path


def _firefox_logger(log_path):
    log_regexp = re.compile(r'<log>.*?</log>')
    log_msg_regexp = re.compile(r'<log>'
                                r'<class>(?P<class>.*?)</class>'
                                r'(<cat>(?P<cat>.*?)</cat>)?'
                                r'<msg>(?P<msg>.*?)</msg>.*?'
                                r'</log>')

    def _get_log(log_type):
            # driver.get_log(name) is bugged for firefox,
            # so instead logs are written to file using
            # consoleExport and firebug, so we read it, parse to json,
            # format and append to end of logs
            if log_type == 'browser':
                console_logs = []
                with open(log_path) as f:
                    for log in log_regexp.finditer(''.join(f.readlines())):
                        log_info = log_msg_regexp.match(log.group(0))
                        if log_info:
                            level = log_info.group('class')
                            cat = log_info.group('cat')
                            if cat and (cat.upper() in level.upper()):
                                level = cat
                            msg = log_info.group('msg')

                            formatted_log = {'timestamp': time.time() * 1000,
                                             'message': msg,
                                             'source': 'console-api',
                                             'level': level.upper()}
                            console_logs.append(formatted_log)
                return console_logs
            else:
                return []

    return _get_log
