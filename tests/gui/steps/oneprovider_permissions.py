"""Steps implementation for permission GUI tests. 
""" 
  
__author__ = "Michal Stanisz" 
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH" 
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt" 

from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.common.modals.edit_permissions import EditPermissionsModal

from selenium.webdriver.support.ui import WebDriverWait as Wait
from pytest_bdd import parsers, when, then


@when(parsers.parse('user of {browser_id} selects {permission_type} '
                    'permission type in active modal'))
@then(parsers.parse('user of {browser_id} selects {permission_type} '
                    'permission type in active modal'))
def select_permission_type(selenium, browser_id, permission_type, modals):
    modals(selenium[browser_id]).edit_permissions.select(permission_type)
   

@when(parsers.parse('user of {browser_id} sees that current permission is "{perm}"'))
@then(parsers.parse('user of {browser_id} sees that current permission is "{perm}"'))
def check_permission(selenium, browser_id, perm, modals):
    input_box = modals(selenium[browser_id]).edit_permissions.get_input_box()
    assert input_box.get_attribute('value') == perm


@when(parsers.parse('user of {browser_id} sets {perm} permission code in active modal'))
@then(parsers.parse('user of {browser_id} sets {perm} permission code in active modal'))
def set_permission(selenium, browser_id, perm, modals):
    input_box = modals(selenium[browser_id]).edit_permissions.get_input_box()
    input_box.clear()
    input_box.send_keys(perm)

