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
from pytest_selenium_multi.drivers.utils import factory


SELENIUM_IMPLICIT_WAIT = 8

# use this const when using: WebDriverWait(selenium, WAIT_FRONTEND).until(lambda s: ...)
# when waiting for frontend changes
WAIT_FRONTEND = SELENIUM_IMPLICIT_WAIT

# use this const when using: WebDriverWait(selenium, WAIT_BACKEND).until(lambda s: ...)
# when waiting for backend changes
WAIT_BACKEND = 15

# waiting for backend to load after refresh
WAIT_REFRESH = WAIT_BACKEND
MAX_REFRESH_COUNT = 6

# name of browser currently being created create_instances_of_webdriver in common.py
browser_being_created = ''


def get_global_browser_being_created():
    global browser_being_created
    return browser_being_created


def set_global_browser_being_created(val):
    global browser_being_created
    browser_being_created = val


cmd_line = ' '.join(sys.argv)
is_base_url_provided = re.match(r'.*--base-url=.*', cmd_line)
is_firefox_logging_enabled = re.match(r'.*--firefox-logs.*', cmd_line)
is_recording_enabled = re.match(r'.*--xvfb-recording(?!\s*=?\s*none).*', cmd_line)


@pytest.fixture(scope='session')
def oz_page():
    from tests.gui.utils.onezone_gui import OZLoggedIn
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
    from pytest_selenium_multi.safety import sensitive_url as orig_sensitive_url
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
    capabilities['loggingPrefs'] = {'browser': 'ALL'}
    capabilities['acceptSslCerts'] = True

    # uncomment to debug selenium browser init
    # print "DEBUG: Current capabilities: ", capabilities

    return capabilities


@pytest.fixture
def firefox_profile(firefox_profile, tmpdir):
    @factory
    def _get_instance():
        profile = firefox_profile.get_instance()
        profile.set_preference('browser.download.folderList', 2)
        profile.set_preference('browser.download.manager.showWhenStarting',
                               False)
        profile.set_preference('browser.helperApps.alwaysAsk.force', False)
        profile.set_preference('browser.download.dir', str(tmpdir))
        profile.set_preference('browser.helperApps.neverAsk.saveToDisk',
                               'text/anytext, text/plain, text/html')

        if is_firefox_logging_enabled:
            gui_tests_dir = os.path.dirname(os.path.abspath(gui.__file__))
            firebug_path = os.path.join(gui_tests_dir, 'firefox_extensions',
                                        'firebug-2.0.17.xpi')
            profile.add_extension(firebug_path)
            console_exp_path = os.path.join(gui_tests_dir, 'firefox_extensions',
                                            'consoleExport-0.5b5.xpi')
            profile.add_extension(console_exp_path)

            profile.set_preference('extensions.firebug.consoleexport.active',
                                   'true')
            profile.set_preference('extensions.firebug.consoleexport.logFilePath',
                                   '{root_dir}/{browser_id}/logs/firefox.log'
                                   ''.format(root_dir=str(tmpdir),
                                             browser_id=get_global_browser_being_created()))

            profile.set_preference('extensions.firebug.framePosition',
                                   'detached')
            profile.set_preference("extensions.firebug.currentVersion", "2.0")
            profile.set_preference("extensions.firebug.console.enableSites",
                                   'true')
            profile.set_preference("extensions.firebug.net.enableSites",
                                   'true')
            profile.set_preference("extensions.firebug.script.enableSites",
                                   'true')
            profile.set_preference("extensions.firebug.allPagesActivation",
                                   'on')

        profile.update_preferences()
        return profile

    _get_instance.browser = None
    return _get_instance


@pytest.fixture
def browser_width():
    return 1280


@pytest.fixture
def browser_height():
    return 1024


# TODO: configure different window sizes for responsiveness tests: https://jira.plgrid.pl/jira/browse/VFS-2205
@pytest.fixture
def config_driver(config_driver, browser_width, browser_height):
    def _configure(driver):
        driver = config_driver(driver)
        driver.implicitly_wait(SELENIUM_IMPLICIT_WAIT)
        driver.set_window_size(browser_width, browser_height)
        # currenlty, we rather set window size
        # driver.maximize_window()
        return driver
    return _configure


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


if is_recording_enabled:
    @pytest.yield_fixture(autouse=True)
    def xvfb_recording(request, xvfb, browser_width, browser_height):

        file_name = '{name}.mp4'.format(name=request.node.name)
        logdir = os.path.dirname(request.config.option.htmlpath)
        asset_path = os.path.join(logdir, 'movies', file_name)
        if not os.path.exists(os.path.dirname(asset_path)):
            os.makedirs(os.path.dirname(asset_path))

        cmd = ['ffmpeg', '-f', 'x11grab', '-video_size',
               '{width}x{height}'.format(width=browser_width,
                                         height=browser_height),
               '-i', ':{display}'.format(display=xvfb.display),
               '-codec:v', 'libx264', '-r', '25',
               '{path}'.format(path=asset_path)]

        dev_null = open(os.devnull, 'w')
        p = sp.Popen(cmd, stdin=sp.PIPE, stdout=dev_null, stderr=dev_null)

        yield

        p.communicate(input=b'q')
        if not p.stdin.closed:
            p.stdin.close()


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
