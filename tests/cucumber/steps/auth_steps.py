"""
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements pytest-bdd steps for authorization and mounting oneclient.
"""
from pytest_bdd import given

import multi_auth_steps
from cucumber_utils import *
from tests.utils.client_utils import mount_users


@given(parsers.parse('{user} starts oneclient in {mount_path} using {token}'))
def default_mount(user, mount_path, token, request, environment, context,
                  client_ids, env_description_file):
    mount_users(request, environment, context, client_ids, env_description_file,
                users=[user], client_instances=["client1"],
                mount_paths=[mount_path], client_hosts=['client-host1'],
                tokens=[token])


@given(parsers.parse('oneclient is started for {user}'))
def is_oneclient_started(user, context):
    multi_auth_steps.is_oneclient_started_multi(make_arg_list(user),
                                                make_arg_list('client1'),
                                                context)
