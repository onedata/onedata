"""
Define fixtures used in web GUI acceptance/behavioral tests.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.utils.utils import set_dns
from tests.utils.path_utils import make_logdir
from tests.conftest import map_test_type_to_logdir
from pytest import fixture
from selenium import webdriver
import pytest
import re

import sys


SELENIUM_IMPLICIT_WAIT = 15

# use this const when using: WebDriverWait(selenium, WAIT_FRONTEND).until(lambda s: ...)
# when waiting for frontend changes
WAIT_FRONTEND = SELENIUM_IMPLICIT_WAIT

# use this const when using: WebDriverWait(selenium, WAIT_BACKEND).until(lambda s: ...)
# when waiting for backend changes
WAIT_BACKEND = 15

# waiting for backend to load after refresh
WAIT_REFRESH = 2 * WAIT_BACKEND

cmd_line = ' '.join(sys.argv)
is_base_url_provided = re.match(r'.*--base-url=.*', cmd_line)


@pytest.fixture
def get_url(selenium):
    return selenium.current_url


@pytest.fixture
def clipboard():
    return {}


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


@pytest.fixture(scope='function', autouse=True)
def _skip_sensitive(request, sensitive_url):
    """Invert the default sensitivity behaviour: consider the test as destructive
    only if it has marker "destructive".
    """
    destructive = 'destructive' in request.node.keywords
    if sensitive_url and destructive:
        pytest.skip(
            'This test is destructive and the target URL is '
            'considered a sensitive environment. If this test is '
            'not destructive, add the \'nondestructive\' marker to '
            'it. Sensitive URL: {0}'.format(sensitive_url))


@pytest.fixture
def capabilities(request, capabilities, tmpdir):
    """Add --no-sandbox argument for Chrome headless
    Should be the same as adding capability: 'chromeOptions': {'args': ['--no-sandbox'], 'extensions': []}
    """
    if capabilities is None:
        capabilities = {}

    if 'browserName' in capabilities and capabilities['browserName'] == 'chrome' or request.config.option.driver == 'Chrome':
        chrome_options = webdriver.ChromeOptions()
        # TODO: use --no-sandbox only in headless mode, support for Chrome in Docker and XVFB can be buggy now: https://jira.plgrid.pl/jira/browse/VFS-2204
        chrome_options.add_argument("--no-sandbox")
        chrome_options.add_argument("enable-popup-blocking")
        prefs = {"download.default_directory": str(tmpdir)}
        chrome_options.add_experimental_option("prefs", prefs)
        capabilities.update(chrome_options.to_capabilities())
    # TODO: use Firefox Marionette driver (geckodriver) for Firefox 47: https://jira.plgrid.pl/jira/browse/VFS-2203
    # but currently this driver is buggy...
    # elif 'browserName' in capabilities and capabilities['browserName'] == 'firefox' or request.config.option.driver == 'Firefox':
    #     capabilities['marionette'] = True

    # currently there are no problems with invalid SSL certs in built-in FF driver and Chrome
    # but some drivers could need it
    capabilities['acceptSslCerts'] = True

    # uncomment to debug selenium browser init
    # print "DEBUG: Current capabilities: ", capabilities

    return capabilities


@pytest.fixture
def firefox_profile(firefox_profile, tmpdir):
    firefox_profile.set_preference('browser.download.folderList', 2)
    firefox_profile.set_preference('browser.download.manager.showWhenStarting',
                                   False)
    firefox_profile.set_preference('browser.helperApps.alwaysAsk.force', False)
    firefox_profile.set_preference('browser.download.dir', str(tmpdir))
    firefox_profile.set_preference('browser.helperApps.neverAsk.saveToDisk',
                                   'text/anytext, text/plain, text/html')
    firefox_profile.update_preferences()
    return firefox_profile


# TODO: configure different window sizes for responsiveness tests: https://jira.plgrid.pl/jira/browse/VFS-2205
@pytest.fixture
def selenium(selenium):
    selenium.implicitly_wait(SELENIUM_IMPLICIT_WAIT)
    selenium.set_window_size(1280, 1024)
    # currenlty, we rather set window size
    # selenium.maximize_window()
    return selenium
