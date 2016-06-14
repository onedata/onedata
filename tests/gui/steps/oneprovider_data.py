"""Steps for features of Onezone login page.
"""
from tests.gui.conftest import WAIT_BACKEND
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

