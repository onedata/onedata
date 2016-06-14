"""Steps for features of Onezone login page.
"""
from tests.gui.conftest import WAIT_BACKEND
from tests.utils.cucumber_utils import list_parser

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
import re
from tests.gui.conftest import WAIT_FRONTEND
from pytest_bdd import given, then
from pytest_bdd import parsers
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys
from pytest_bdd import given, when, then, parsers
from tests.gui.utils.generic import parse_url


@when(parsers.re(r'I change the space to "(?P<space_name>.+)"'))
def change_space(selenium, space_name):
    selenium.find_element_by_css_selector('.data-spaces-select a[data-toggle=dropdown]').click()
    spaces = selenium.find_elements_by_css_selector('.data-spaces-select .dropdown-menu a')
    def space_by_name(s):
        named_spaces = [s for s in spaces if s.text().strip() == space_name]
        if len(named_spaces) > 0:
            return named_spaces[0]
        else:
            return None
    Wait(selenium, WAIT_FRONTEND).until(EC.element_to_be_clickable(space_by_name)).click()
