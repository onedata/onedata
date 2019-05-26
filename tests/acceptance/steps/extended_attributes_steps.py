"""Module implements common steps for manipulating extended attributes.
"""
__author__ = "Bartek Kryza"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import multi_file_steps
from tests.utils.acceptance_utils import *


@when(parsers.cfparse('{user} sets on {file} extended attribute <name> with value '
                      '<value>'))
@then(parsers.cfparse('{user} sets on {file} extended attribute <name> with value '
                      '<value>'))
def set_extended_attribute(user, file, name, value, context):
    multi_file_steps.set_xattr(user, file, name, value, "client1", context)


@when(parsers.cfparse('{user} removes extended attribute <name> from {file}'))
@then(parsers.cfparse('{user} removes extended attribute <name> from {file}'))
def remove_extended_attribute(user, file, name, context):
    multi_file_steps.remove_xattr(user, file, name, "client1", context)


@when(parsers.cfparse('{user} checks that {file} has extended attribute <name>'))
@then(parsers.cfparse('{user} checks that {file} has extended attribute <name>'))
def check_extended_attribute_exists(user, file, name, context):
    multi_file_steps.check_xattr_exists(user, file, name, "client1", context)


@when(parsers.cfparse('{user} checks that {file} does not have extended '
                      'attribute <name>'))
@then(parsers.cfparse('{user} checks that {file} does not have extended '
                      'attribute <name>'))
def check_extended_attribute_doesn_exist(user, file, name, context):
    multi_file_steps.check_xattr_doesnt_exist(user, file, name, "client1", context)


@when(parsers.cfparse('{user} checks that {file} has string extended attribute '
                      '<name> with value <value>'))
@then(parsers.cfparse('{user} checks that {file} has string extended attribute '
                      '<name> with value <value>'))
def check_string_extended_attribute(user, file, name, value, context):
    multi_file_steps.check_string_xattr(user, file, name, value, "client1", context)


@when(parsers.cfparse('{user} checks that {file} has numeric extended attribute '
                      '<name> with value <value>'))
@then(parsers.cfparse('{user} checks that {file} has numeric extended attribute '
                      '<name> with value <value>'))
def check_numeric_extended_attribute(user, file, name, value, context):
    multi_file_steps.check_numeric_xattr(user, file, name, value, "client1", context)


@when(parsers.cfparse('{user} checks that {file} has JSON extended attribute '
                      '<name> with value <value>'))
@then(parsers.cfparse('{user} checks that {file} has JSON extended attribute '
                      '<name> with value <value>'))
def check_json_extended_attribute(user, file, name, value, context):
    multi_file_steps.check_json_xattr(user, file, name, value, "client1", context)
