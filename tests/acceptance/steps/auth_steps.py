"""Module implements pytest-bdd steps for authorization and mounting oneclient.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import multi_auth_steps
from tests.utils.acceptance_utils import *
from tests.utils.client_utils import mount_users

from pytest_bdd import given


@given(parsers.re('(?P<user>\w+) starts oneclient in (?P<mount_path>.*) using (?P<token>.*)'))
def default_mount(user, mount_path, token, request, onedata_environment,
                  context, client_dockers, env_description_abs_path, providers):
    mount_users(request, onedata_environment, context, client_dockers,
                env_description_abs_path, providers, user_names=[user],
                client_instances=["client1"], mount_paths=[mount_path],
                client_hosts=['client-host1'], tokens=[token])


@when(parsers.re('(?P<spaces>.*) is mounted for (?P<user>\w+)'))
@then(parsers.re('(?P<spaces>.*) is mounted for (?P<user>\w+)'))
@when(parsers.re('(?P<spaces>.*) are mounted for (?P<user>\w+)'))
@then(parsers.re('(?P<spaces>.*) are mounted for (?P<user>\w+)'))
def check_spaces(spaces, user, context):
    multi_auth_steps.check_spaces(spaces, user, make_arg_list("client1"),
                                  context)


@when(parsers.re('(?P<user>\w+) remounts oneclient'))
@then(parsers.re('(?P<user>\w+) remounts oneclient'))
def remount_client(user, context):
    multi_auth_steps.remount_client(user, 'client1', context)
