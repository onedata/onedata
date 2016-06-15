"""Steps for features of Onezone login page.
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.generic import current_dir
from pytest_bdd import when, parsers
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
# from selenium.webdriver.common.keys import Keys
# from pytest_bdd import given, when, then, parsers
# from tests.gui.utils.generic import parse_url


@when(parsers.re(r'I change the space to "(?P<space_name>.+)"'))
def change_space(selenium, space_name):
    # TODO: not working in Firefox
    Wait(selenium, WAIT_FRONTEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, '.data-spaces-select a[data-toggle=dropdown]'))
    ).click()
    spaces = selenium.find_elements_by_css_selector('.data-spaces-select .dropdown-menu a')
    def space_by_name(_):
        named_spaces = [s for s in spaces if re.match(space_name, s.text.strip(), re.I)]
        if len(named_spaces) > 0 and named_spaces[0].is_enabled():
            return named_spaces[0]
        else:
            return None
    Wait(selenium, WAIT_FRONTEND).until(space_by_name).click()


@when('The current dir has no write permissions')
def remove_write_permissions(selenium):
    # FIXME
    print "current dir", current_dir(selenium)
