"""Steps for features of Onezone login page.
"""
from tests.gui.conftest import WAIT_BACKEND
from tests.gui.steps import onezone_logged_in_common as onezone_session
from tests.gui.steps import onezone_before_login as onezone_no_session
from tests.utils.cucumber_utils import list_parser

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
import re
from pytest_bdd import given, then
from pytest_bdd import parsers
from selenium.webdriver.support.ui import WebDriverWait as wait
from selenium.webdriver.common.keys import Keys
from pytest_bdd import given, when, then, parsers
from tests.gui.utils.generic import parse_url


@given(parsers.parse('I\'m logged into Oneprovider "{provider}" as development user "{user}"'))
def logged_in_dev_to_oneprovider(selenium, base_url, user, provider):
    onezone_no_session.login_dev_onezone_with_url(selenium, base_url, user)
    onezone_session.uncollapse_main_accordion(selenium, 'providers')
    onezone_session.go_to_provider(selenium, provider)
    pass
