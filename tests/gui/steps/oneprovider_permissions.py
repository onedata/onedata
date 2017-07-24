"""Steps implementation for permissions GUI tests. 
""" 
  
__author__ = "Michal Stanisz" 
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH" 
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt" 

import random
import string
from pytest_bdd import parsers, when, then
from selenium.webdriver.support.ui import WebDriverWait as Wait


@when(parsers.parse('user of {browser_id} selects {permission_type} '
                    'permission type in active modal'))
@then(parsers.parse('user of {browser_id} selects {permission_type} '
                    'permission type in active modal'))
def select_permission_type(selenium, browser_id, permission_type, modals):
    modals(selenium[browser_id]).edit_permissions.select(permission_type)
   

@when(parsers.parse('user of {browser_id} sees that current permission is '
                    '"{perm}"'))
@then(parsers.parse('user of {browser_id} sees that current permission is '
                    '"{perm}"'))
def check_permission(selenium, browser_id, perm, modals):
    assert modals(selenium[browser_id]).edit_permissions.posix.value == perm


@when(parsers.parse('user of {browser_id} sets "{perm}" permission code in '
                    'active modal'))
@then(parsers.parse('user of {browser_id} sets "{perm}" permission code in '
                    'active modal'))
def set_permission(selenium, browser_id, perm, modals):
    modals(selenium[browser_id]).edit_permissions.posix.value = perm


@when(parsers.parse('user of {browser_id} sees that "{name}" confirmation '
                    'button in displayed modal is disabled'))
@then(parsers.parse('user of {browser_id} sees that "{name}" confirmation '
                    'button in displayed modal is disabled'))
def button_in_modal_is_disabled(browser_id, name, tmp_memory):
    modal = tmp_memory[browser_id]['window']['modal']
    buttons = modal.find_elements_by_css_selector('button')
    
    for btn in buttons:
        if btn.text.lower() == name.lower():
            assert btn.get_attribute('disabled')
            break
    else:
        raise RuntimeError('no button named {} found'.format(name))


@when(parsers.parse('user of {browser_id} sets incorrect {num:d} char '
                    'permission code in active modal'))
@then(parsers.parse('user of {browser_id} sets incorrect {num:d} char '
                    'permission code in active modal'))
def set_incorect_permission(selenium, browser_id, num, modals):
    random.seed()
    val = random.choice('89')
    for _ in range(num-1):
        val += random.choice(string.digits)
    modals(selenium[browser_id]).edit_permissions.posix.value = val
