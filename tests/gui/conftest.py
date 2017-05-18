"""
Define fixtures used in web GUI acceptance/behavioral tests.
"""

import os
import re
import sys
from itertools import chain
import subprocess as sp

from py.xml import html
from pytest import fixture, UsageError, skip
from selenium import webdriver

from environment import docker
from tests.utils.utils import set_dns
from tests.utils.path_utils import make_logdir
from tests.conftest import map_test_type_to_logdir


__author__ = "Jakub Liput, Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


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

html.__tagspec__.update({x: 1 for x in ('video', 'source')})
VIDEO_ATTRS = {'controls': '',
               'poster': '',
               'play-pause-on-click': '',
               'style': 'border:1px solid #e6e6e6; '
                        'float:right; height:240px; '
                        'margin-left:5px; overflow:hidden; '
                        'width:320px'}


is_base_url_provided = re.match(r'.*--base-url=.*', ' '.join(sys.argv))


def pytest_configure(config):
    """Set default path for Selenium HTML report if explicit '--html=' not specified"""
    htmlpath = config.option.htmlpath
    if htmlpath is None:
        import os
        logdir = make_logdir(map_test_type_to_logdir('gui'), 'report')
        config.option.htmlpath = os.path.join(logdir, 'report.html')


def pytest_addoption(parser):
    group = parser.getgroup('selenium', 'selenium')
    group.addoption('--firefox-logs',
                    action='store_true',
                    help='enable firefox console logs using firebug')

    group = parser.getgroup('onedata', description='option specific '
                                                   'to onedata tests')
    group.addoption('--user', action='append', default=[], nargs=2,
                    help='user credentials in form: -u username password',
                    metavar=('username', 'password'), dest='users')

    group.addoption('--oneprovider-host', action='append', default=[], nargs=2,
                    help='IP address of oneprovider in form: alias address',
                    metavar=('alias', 'address'), dest='oneprovider')
    group.addoption('--op-panel-host', action='append', default=[], nargs=2,
                    help='IP address of op-panel in form: alias address',
                    metavar=('alias', 'address'), dest='provider_panel')
    group.addoption('--onezone-host', action='append', default=[], nargs=2,
                    help='IP address of onezone in form: alias address',
                    metavar=('alias', 'address'), dest='onezone')
    group.addoption('--oz-panel-host', action='append', default=[], nargs=2,
                    help='IP address of oz-panel in form: alias address',
                    metavar=('alias', 'address'), dest='zone_panel')


@fixture(scope='module')
def hosts(request):
    """Dict to use to store ip addresses of services."""
    return {service: {alias: ip for alias, ip
                      in request.config.getoption(service)}
            for service in ('oneprovider', 'provider_panel',
                            'onezone', 'zone_panel')}


@fixture(scope='module')
def users(request):
    """Dict to use to store user credentials."""
    credentials = request.config.getoption('users')
    return {user: password for user, password in credentials}


def pytest_selenium_capture_debug(item, report, extra):
    recording = item.config.getoption('--xvfb-recording')
    if recording == 'none' or (recording == 'fail' and not report.failed):
        return

    log_dir = os.path.dirname(item.config.option.htmlpath)
    pytest_html = item.config.pluginmanager.getplugin('html')
    for movie_path in item._movies:
        src_attrs = {'src': os.path.relpath(movie_path, log_dir),
                     'type': 'video/mp4'}
        video_html = str(html.video(html.source(**src_attrs), **VIDEO_ATTRS))
        extra.append(pytest_html.extras.html(video_html))


@fixture(scope='session')
def driver_type(request):
    return request.config.getoption('--driver')


@fixture(scope='session')
def firefox_logging(request, driver_type):
    enabled = request.config.getoption('--firefox-logs')
    if enabled and driver_type.lower() != 'firefox':
        raise UsageError('--driver=Firefox must be specified '
                         'if --firefox-logs option is given')
    return enabled


@fixture(scope='session')
def cdmi():
    from tests.gui.utils.oneservices.cdmi import CDMIClient
    return CDMIClient


@fixture(scope='session')
def onepage():
    from tests.gui.utils.common.common import OnePage
    return OnePage


@fixture(scope='session')
def onepanel():
    from tests.gui.utils.onepanel import Onepanel
    return Onepanel


@fixture(scope='session')
def login_page():
    from tests.gui.utils.common.login import LoginPage
    return LoginPage


@fixture(scope='session')
def oz_page():
    from tests.gui.utils.onezone import OZLoggedIn
    return OZLoggedIn


@fixture(scope='session')
def op_page():
    from tests.gui.utils.oneprovider_gui import OPLoggedIn
    return OPLoggedIn


@fixture(scope='session')
def modals():
    from tests.gui.utils.common.modals import Modals
    return Modals


@fixture(scope='session')
def popups():
    from tests.gui.utils.common.popups import Popups
    return Popups


@fixture(scope='module')
def screen_width():
    return 1366


@fixture(scope='module')
def screen_height():
    return 1024


@fixture(scope='module')
def movie_dir(request):
    log_dir = os.path.dirname(request.config.option.htmlpath)
    movie_subdir = os.path.join(log_dir, 'movies')
    if not os.path.exists(movie_subdir):
        os.makedirs(movie_subdir)
    return movie_subdir


@fixture(scope='module')
def screens(request):
    _, mod = os.path.split(request.node.name)
    match = re.match(r'\w+_browsers_(?P<num>\d+).py$', mod)
    try:
        num = int(match.group('num'))
    except AttributeError:
        return [0]
    else:
        return [i for i in range(num)]


@fixture
def tmp_memory():
    """Dict to use when one wants to store sth between steps.

    Because of use of multiple browsers, the correct format would be:
     {'browser1': {...}, 'browser2': {...}, ...}
    """
    return {}


@fixture
def displays():
    """Dict mapping browser to used display (e.g. {'browser1': ':0.0'} )"""
    return {}


@fixture(scope='session')
def clipboard():
    """utility simulating os clipboard"""
    from collections import namedtuple
    cls = namedtuple('Clipboard', ['copy', 'paste'])

    def copy(text, display):
        p = sp.Popen(['xclip', '-d', display, '-selection', 'c'],
                     stdin=sp.PIPE, close_fds=True)
        p.communicate(input=text.encode('utf-8'))

    def paste(display):
        p = sp.Popen(['xclip', '-d', display, '-selection', 'c', '-o'],
                     stdout=sp.PIPE, close_fds=True)
        stdout, _ = p.communicate()
        return stdout.decode('utf-8')

    return cls(copy, paste)


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


@fixture(scope='module', autouse=True)
def _verify_url(request, base_url):
    """Override original fixture to change scope to module (we can have different base_urls for each module)"""
    from pytest_base_url.plugin import _verify_url as orig_verify_url
    return orig_verify_url(request, base_url)


@fixture(scope='module', autouse=True)
def sensitive_url(request, base_url):
    """Override original fixture to change scope to module (we can have different base_urls for each module)"""
    from pytest_selenium.safety import sensitive_url as orig_sensitive_url
    return orig_sensitive_url(request, base_url)


@fixture(scope='function', autouse=True)
def _skip_sensitive(request, sensitive_url):
    """Invert the default sensitivity behaviour: consider the test as destructive
    only if it has marker "destructive".
    """
    destructive = 'destructive' in request.node.keywords
    if sensitive_url and destructive:
        skip('This test is destructive and the target URL is '
             'considered a sensitive environment. If this test is '
             'not destructive, add the \'nondestructive\' marker to '
             'it. Sensitive URL: {0}'.format(sensitive_url))


@fixture
def capabilities(request, capabilities, tmpdir):
    """Add --no-sandbox argument for Chrome headless
    Should be the same as adding capability: 'chromeOptions': {'args': ['--no-sandbox'], 'extensions': []}
    """
    if capabilities is None:
        capabilities = {}

    if 'browserName' in capabilities and capabilities['browserName'] == 'chrome' or request.config.option.driver == 'Chrome':
        options = webdriver.ChromeOptions()
        options.add_argument("--no-sandbox")
        options.add_argument("enable-popup-blocking")
        prefs = {"download.default_directory": str(tmpdir)}
        options.add_experimental_option("prefs", prefs)
        capabilities.update(options.to_capabilities())
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
    @fixture(scope="module", autouse=True)
    def _logging_environment(persistent_environment):
        cmd = r'echo {\"debug\": true} > ' \
              r'/root/bin/node/data/gui_static/app-config.json'

        for worker in chain(persistent_environment['op_worker_nodes'],
                            persistent_environment['oz_worker_nodes']):
            docker_name = worker.split('@')[1]
            docker.exec_(docker_name, cmd)


def pytest_collection_modifyitems(items):
    first = []
    last = []
    rest = []
    run_first = ('test_cluster_deployment',
                 'test_user_can_unsupport_space',
                 'test_user_sees_that_after_unsupporting_space'
                 )
    run_last = ('test_user_sees_that_when_no_provider_is_working_'
                'appropriate_msg_is_shown',
                'test_revoke_space_support',
                'test_user_deregisters_provider')

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
