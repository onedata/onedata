"""Steps implementation for permissions GUI tests. 
""" 
  
__author__ = "Michal Stanisz" 
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH" 
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt" 

import random
import string
from pytest_bdd import parsers, when, then
from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed


@when(parsers.parse('user of {browser_id} selects "{permission_type}" '
                    'permission type in active modal'))
@then(parsers.parse('user of {browser_id} selects "{permission_type}" '
                    'permission type in active modal'))
@repeat_failed(timeout=WAIT_FRONTEND)
def select_permission_type(selenium, browser_id, permission_type, modals):
    modals(selenium[browser_id]).edit_permissions.select(permission_type)
   

@when(parsers.parse('user of {browser_id} sees that current permission is '
                    '"{perm}"'))
@then(parsers.parse('user of {browser_id} sees that current permission is '
                    '"{perm}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def check_permission(selenium, browser_id, perm, modals):
    perm_value = modals(selenium[browser_id]).edit_permissions.posix.value 
    assert perm_value == perm, "POSIX permission value {} instead of'\
                               ' expected {}".format(perm_value, perm)


@when(parsers.parse('user of {browser_id} sets "{perm}" permission code in '
                    'active modal'))
@then(parsers.parse('user of {browser_id} sets "{perm}" permission code in '
                    'active modal'))
@repeat_failed(timeout=WAIT_FRONTEND)
def set_permission(selenium, browser_id, perm, modals):
    modals(selenium[browser_id]).edit_permissions.posix.value = perm


@when(parsers.parse('user of {browser_id} sets incorrect {num:d} char '
                    'permission code in active modal'))
@then(parsers.parse('user of {browser_id} sets incorrect {num:d} char '
                    'permission code in active modal'))
@repeat_failed(timeout=WAIT_FRONTEND)
def set_incorect_permission(selenium, browser_id, num, modals):
    random.seed()
    val = random.choice('89')
    for _ in range(num-1):
        val += random.choice(string.digits)
    modals(selenium[browser_id]).edit_permissions.posix.value = val
