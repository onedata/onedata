"""This module implements some common basic functions and functionality for
acceptance tests of onedata.
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
    assert not context.get_user(user).last_operation_failed


@when(parsers.parse('last operation by {user} fails'))
@then(parsers.parse('last operation by {user} fails'))
def failure(user, context):
    assert context.get_user(user).last_operation_failed


###################### FUNCTIONS ######################


def list_parser(list):
    return [el.strip() for el in list.strip("[]").split(',') if el != ""]


def make_arg_list(arg):
    return "[" + arg + "]"

