"""Module implements pytest-bdd steps for authorization and mounting oneclient.
"""
__author__ = "Jakub Kudzia, Piotr Ociepka"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.utils.utils import assert_
from tests.utils.client_utils import ls, mount_users
from tests.utils.acceptance_utils import *

from pytest_bdd import given


@given(parsers.re('(?P<users>.*) start oneclients (?P<client_instances>.*) in\n' +
                     '(?P<mount_paths>.*) on client_hosts\n' +
                     '(?P<client_hosts>.*) respectively,\n' +
                     'using (?P<tokens>.*)'))
def multi_mount(users, client_instances, mount_paths, client_hosts, tokens,
                request, onedata_environment, context, client_dockers,
                env_description_abs_path, providers):
    mount_users(request, onedata_environment, context, client_dockers,
                env_description_abs_path, providers, user_names=list_parser(users),
                client_instances=list_parser(client_instances),
                mount_paths=list_parser(mount_paths),
                client_hosts=list_parser(client_hosts),
                tokens=list_parser(tokens))


@then(parsers.re('(?P<spaces>.*) are mounted for (?P<user>\w+) on (?P<client_nodes>.*)'))
def check_spaces(spaces, user, client_nodes, context):
    spaces = list_parser(spaces)
    user = str(user)
    client_nodes = list_parser(client_nodes)

    for client_node in client_nodes:
        client = context.get_client(user, client_node)

        def condition():
            spaces_in_client = ls(client, path=client.mount_path)
            for space in spaces:
                assert space in spaces_in_client

        assert_(client.perform, condition)


@when(parsers.re('(?P<user>\w+) remounts oneclient (?P<client_node>.*)'))
@then(parsers.re('(?P<user>\w+) remounts oneclient (?P<client_node>.*)'))
def remount_client(user, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    assert client.remount(user) == 0
