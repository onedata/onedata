"""
Author: Piotr Ociepka
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements pytest-bdd steps for authorization and mounting oneclient.
"""
import subprocess

from pytest_bdd import given

import multi_file_steps
from cucumber_utils import *
from tests.utils.client_utils import ls, mount_users


@given(parsers.parse('{users} start oneclients {client_instances} in\n' +
                     '{mount_paths} on client_hosts\n' +
                     '{client_hosts} respectively,\n' +
                     'using {tokens}'))
def multi_mount(users, client_instances, mount_paths, client_hosts, tokens,
                request, environment, context, client_ids,
                env_description_file):
    mount_users(users, client_instances, mount_paths, client_hosts, tokens,
                request, environment, context, client_ids,
                env_description_file)


@then(parsers.parse('{spaces} are mounted for {user}'))
def check_spaces(spaces, user, context):
    # sleep to be sure that environment is up
    time.sleep(10)
    spaces = list_parser(spaces)
    user = str(user)
    for client_instance, client in context.users[user].clients.items():
        spaces_path = make_path("spaces", client)

        def condition():

            try:
                spaces_in_client = ls(client, path=spaces_path, user=user)
                spaces_in_client = spaces_in_client.split("\n")
                for space in spaces:
                    if space not in spaces_in_client:
                        return False
                    return True
            except subprocess.CalledProcessError:
                return False

        assert repeat_until(condition, timeout=client.timeout)


# TODO is this step needed ???
@given(parsers.parse('oneclient is started for {users} on {clients}'))
def is_oneclient_started_multi(users, clients, context):
    users = list_parser(users)
    clients = list_parser(clients)
    params = zip(users, clients)
    for user, client in params:
        multi_file_steps.ls_present(user, make_arg_list('spaces'), '.', client,
                                    context)
