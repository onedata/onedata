"""This module implements some common basic functions and functionality for
cucumber-like tests of onedata.
"""
__author__ = "Jakub Kudzia, Piotr Ociepka"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import time

from pytest_bdd import parsers
from pytest_bdd import when, then


######################## STEPS ########################

@when(parsers.parse('{user} waits {seconds} second'))
@then(parsers.parse('{user} waits {seconds} second'))
@when(parsers.parse('{user} waits {seconds} seconds'))
@then(parsers.parse('{user} waits {seconds} seconds'))
def user_wait_default(user, seconds):
    time.sleep(int(seconds))


@when(parsers.parse('last operation by {user} succeeds'))
@then(parsers.parse('last operation by {user} succeeds'))
def success(user, context):
    assert context.users[user].last_op_ret_code == 0


@when(parsers.parse('last operation by {user} fails'))
@then(parsers.parse('last operation by {user} fails'))
def failure(user, context):
    assert context.users[user].last_op_ret_code != 0


###################### FUNCTIONS ######################


def list_parser(list):
    return [el.strip() for el in list.strip("[]").split(',')]


def make_arg_list(arg):
    return "[" + arg + "]"


def repeat_until(condition, timeout=0):

    condition_satisfied = condition()
    while not condition_satisfied and timeout >= 0:
        print "TIMEOUT: ", timeout
        time.sleep(1)
        timeout -= 1
        condition_satisfied = condition()
    return timeout > 0 or condition_satisfied


