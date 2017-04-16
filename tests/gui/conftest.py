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
from tests import gui
import os
import subprocess as sp

from environment import docker


SELENIUM_IMPLICIT_WAIT = 4

# use this const when using: WebDriverWait(selenium, WAIT_FRONTEND).until(lambda s: ...)
# when waiting for frontend changes
WAIT_FRONTEND = 4

# use this const when using: WebDriverWait(selenium, WAIT_BACKEND).until(lambda s: ...)
# when waiting for backend changes
WAIT_BACKEND = 15

# waiting for backend to load after refresh
WAIT_REFRESH = WAIT_BACKEND
MAX_REFRESH_COUNT = 6


cmd_line = ' '.join(sys.argv)
is_base_url_provided = re.match(r'.*--base-url=.*', cmd_line)


def pytest_addoption(parser):
    group = parser.getgroup('selenium', 'selenium')
    group.addoption('--firefox-logs',
                    action='store_true',
                    help='enable firefox console logs using firebug')


@pytest.fixture(scope='session')
def driver_type(request):
    return request.config.getoption('--driver')


@pytest.fixture(scope='session')
def firefox_logging(request, driver_type):
    enabled = request.config.getoption('--firefox-logs')
    if enabled and driver_type.lower() != 'firefox':
        raise pytest.UsageError('--driver=Firefox must be specified '
                                'if --firefox-logs option is given')
    return enabled


@pytest.fixture(scope='session')
def cdmi():
    from tests.gui.utils.oneservices.cdmi import CDMIClient
    return CDMIClient


@pytest.fixture(scope='session')
def oz_page():
    from tests.gui.utils.onezone import OZLoggedIn
    return OZLoggedIn


@pytest.fixture(scope='session')
def op_page():
    from tests.gui.utils.oneprovider_gui import OPLoggedIn
    return OPLoggedIn


@pytest.fixture(scope='session')
def modals():
    from tests.gui.utils.common.modals import Modals
    return Modals


@pytest.fixture
def browser_width():
    return 1280


@pytest.fixture
def browser_height():
    return 1024


@pytest.fixture
def tmp_memory():
    """Dict to use when one wants to store sth between steps.

    Because of use of multiple browsers, the correct format would be:
     {'browser1': {...}, 'browser2': {...}, ...}
    """
    return {}


@pytest.fixture(scope='module', autouse=True)
def _verify_url(request, base_url):
    """Override original fixture to change scope to module (we can have different base_urls for each module)"""
    from pytest_base_url.plugin import _verify_url as orig_verify_url
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
        # capabilities['acceptInsecureCerts'] = True
        # capabilities['marionette'] = True

    # currently there are no problems with invalid SSL certs in built-in FF driver and Chrome
    # but some drivers could need it
    capabilities['loggingPrefs'] = {'browser': 'ALL'}
    capabilities['acceptSslCerts'] = True

    # uncomment to debug selenium browser init
    # print "DEBUG: Current capabilities: ", capabilities

    return capabilities


# TODO discover why chrome logs even without this setup using test_run, but doesn't when using ./bamboos/.../env_up.py
if not is_base_url_provided:
    @pytest.fixture(scope="module", autouse=True)
    def logging_environment(persistent_environment):
        cmd = r'echo {\"debug\": true} > ' \
              r'/root/bin/node/data/gui_static/app-config.json'

        for op_worker in persistent_environment['op_worker_nodes']:
            docker_name = op_worker.split('@')[1]
            docker.exec_(docker_name, cmd)

        for oz_worker in persistent_environment['oz_worker_nodes']:
            docker_name = oz_worker.split('@')[1]
            docker.exec_(docker_name, cmd)


def pytest_collection_modifyitems(items):
    first = []
    last = []
    rest = []
    run_first = ('test_user_can_unsupport_space',
                 'test_user_sees_that_after_unsupporting_space'
                 )
    run_last = ('test_user_sees_that_when_no_provider_is_working_'
                'appropriate_msg_is_shown',)

    for item in items:
        for test in run_first:
            if test in item.nodeid:
                first.append(item)
                break
        else:
            for test in run_last:
                if test in item.nodeid:
                    last.append(item)
                    break
            else:
                rest.append(item)

    first.extend(rest)
    first.extend(last)
    items[:] = first
