"""
Define fixtures used in web GUI acceptance/behavioral tests.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.utils.utils import set_dns
from tests.utils.path_utils import env_file, make_logdir, get_file_name
from tests.conftest import map_test_type_to_logdir
from pytest import fixture
from selenium import webdriver
from selenium.webdriver import Firefox, FirefoxProfile
from selenium.webdriver.firefox.firefox_binary import FirefoxBinary
import selenium
import tests
import pytest
import re

import sys


SELENIUM_IMPLICIT_WAIT = 5

# use this conts when using: WebDriverWait(selenium, WAIT_FRONTEND).until(lambda s: ...)
# when waiting for frontend changes
WAIT_FRONTEND = SELENIUM_IMPLICIT_WAIT

# use this conts when using: WebDriverWait(selenium, WAIT_BACKEND).until(lambda s: ...)
# when waiting for backend changes
WAIT_BACKEND = 10


cmd_line = ' '.join(sys.argv)
is_base_url_provided = re.match(r'.*--base-url=.*', cmd_line)


@pytest.fixture(scope='module', autouse=True)
def _verify_url(request, base_url):
    """Override original fixture to change scope to module (we can have different base_urls for each module)"""
    from pytest_selenium.pytest_selenium import _verify_url as orig_verify_url
    return orig_verify_url(request, base_url)


@pytest.fixture(scope='module', autouse=True)
def sensitive_url(request, base_url):
    """Override original fixture to change scope to module (we can have different base_urls for each module)"""
    from pytest_selenium.safety import sensitive_url as orig_sensitive_url
    return orig_sensitive_url(request, base_url)


if not is_base_url_provided:
    @fixture(scope='module')
    def base_url(persistent_environment):
        """
        When --base-url is not provided - set up an environment and get a OZ host.
        Assume, that protocol is always HTTPS, so return base_url: https://<oz_host>
        """
        set_dns(persistent_environment)
        oz_host = re.match(r'worker@node\d*\.(.*)', persistent_environment["oz_worker_nodes"][0]).groups(0)[0]
        return "https://{oz_host}".format(oz_host=oz_host)


def pytest_configure(config):
    """Set default path for Selenium HTML report if explicit '--html=' not specified"""
    htmlpath = config.option.htmlpath
    if htmlpath is None:
        import os
        logdir = make_logdir(map_test_type_to_logdir('gui'), 'report')
        config.option.htmlpath = os.path.join(logdir, 'report.html')


@pytest.fixture
def capabilities(request, capabilities):
    """Add --no-sandbox argument for Chrome headless
    Should be the same as adding capability: 'chromeOptions': {'args': ['--no-sandbox'], 'extensions': []}
    """
    if capabilities is None:
        capabilities = {}

    if 'browserName' in capabilities and capabilities['browserName'] == 'chrome' or request.config.option.driver == 'Chrome':
        chrome_options = webdriver.ChromeOptions()
        # TODO: use --no-sandbox only in headless mode
        chrome_options.add_argument("--no-sandbox")
        capabilities.update(chrome_options.to_capabilities())
    # elif 'browserName' in capabilities and capabilities['browserName'] == 'firefox' or request.config.option.driver == 'Firefox':
    #     capabilities['marionette'] = True

    # capabilities['acceptSslCerts'] = True

    # TODO: debug remove
    print "Current option:", request.config.option.driver
    print "Current capabilities: ", capabilities

    return capabilities


# TODO: configure different window sizes for responsiveness tests
@pytest.fixture
def selenium(selenium):
    selenium.implicitly_wait(SELENIUM_IMPLICIT_WAIT)
    selenium.set_window_size(1280, 1024)
    # currenlty, we rather set window size
    # selenium.maximize_window()
    return selenium


# @pytest.fixture
# def firefox_profile(firefox_profile):
#     firefox_profile.accept_untrusted_certs = True
#     firefox_profile.update_preferences()
#     return firefox_profile


# FIXME: remove
# @pytest.fixture
# def firefox_driver(request, capabilities, driver_path, firefox_profile):
#     """Return a WebDriver using a Firefox instance"""
#     firefox_profile.accept_untrusted_certs = True
#     kwargs = {}
#     if capabilities:
#         kwargs['capabilities'] = capabilities
#     if driver_path is not None:
#         kwargs['executable_path'] = driver_path
#     firefox_path = request.config.getoption('firefox_path')
#     if firefox_path is not None:
#         # get firefox binary from options until there's capabilities support
#         kwargs['firefox_binary'] = FirefoxBinary(firefox_path)
#     kwargs['firefox_profile'] = firefox_profile
#     return Firefox(**kwargs)


# TODO: does not work because of "env" error
# TODO: maybe in other test modules we should choose environments to run
# # In GUI tests use Onedata environment defined for GUI
# @pytest.fixture(scope="module",
#                 params=["single_oz_single_op_env"])
# def env_description_file(request):
#     return env_file(tests.GUI_ENV_DIR, request.param)
