"""Module implements pytest-bdd steps for user management via REST
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
from tests import *
from tests.cucumber.steps import multi_auth_steps
from tests.cucumber.steps.cucumber_utils import list_parser, make_arg_list
from tests.utils.client_utils import mount_users
from tests.utils.utils import set_dns
from tests.utils.user_utils import (create_user, delete_user, authorize_user,
                                    get_provider_certs, get_id)

from environment import docker

from pytest_bdd import given, when, then, parsers

import shutil


@given(parsers.parse("users {users} register with passwords {passwords}"))
def registered_users(users, passwords, context, environment, request):
    set_dns(environment)
    users = list_parser(users)
    passwords = list_parser(passwords)
    onepanel = environment['onepanel_nodes'][0].split('@')[1]

    if not hasattr(context, "users"):
        context.users = {}

    for user_name, password in zip(users, passwords):
        user = create_user(user_name, password, onepanel)
        context.users[user_name] = user

    def fin():
        for user_name in users:
            delete_user(user_name, onepanel)
            # todo przypilnowac usuwania space'ow zeby usunely sie przed uzytkownikami
            del context.users[user_name]

    request.addfinalizer(fin)
    return users


@given(parsers.parse('users {users} have authorization tokens'))
def authorization_tokens(users, context, environment):
    users = list_parser(users)
    for user in users:
        user = context.get_user(user)
        user.set_oz_domain(environment)
        token = authorize_user(user)
        user.tokens['auth'] = token
        user.headers = dict(DEFAULT_HEADERS).update({'token': token})


@given(parsers.parse('users {users} authorize with {provider_ids} certs'))
def provider_certs(users, provider_ids, context, environment,
                   env_description_file, request):

    users = context.get_users(list_parser(users))
    provider_ids = list_parser(provider_ids)

    cert_dirs = {}

    for user, provider_id in zip(users, provider_ids):
        user.provider_id = provider_id
        user.set_op_domain(environment, provider_id)
        user.set_op_worker(environment, user.op_domain)

        cert_dirs[user.name] = get_provider_certs(user, provider_id)

    def fin():
        for user in users:
            shutil.rmtree(cert_dirs[user.name])
    request.addfinalizer(fin)

    return users # todo needed??


@given(parsers.parse('users {users} know their ids'))
def user_ids(users, context):
    users = context.get_users(list_parser(users))
    for user in users:
        user_id = get_id(user)
        user.id = user_id


# TODO this step is slightly different than one in auth_steps
# TODO difference is in list of ids
# TODO find way to use ids in general step (now user name is used as id)
# TODO maybe we should implement users and clients fixtures that would
# TODO be maps of suitable objects, initialized basing on env_up ouptut
@given(parsers.parse(
        '{user} starts oneclient in {mount_path} using {token} on {client_node}'))
def mount(user, mount_path, token, client_node, request, environment, context,
          client_ids,
          env_description_file):
    mount_users(request, environment, context, client_ids, env_description_file,
                users=[user], ids=[context.get_user(user).id],
                client_instances=[client_node], mount_paths=[mount_path],
                client_hosts=['client-host1'], tokens=[token], check=False)


@given(parsers.parse('{users} start oneclients {client_instances} in\n' +
                     '{mount_paths} on client_hosts\n' +
                     '{client_hosts} respectively,\n' +
                     'using {tokens}'))
def multi_mount(users, client_instances, mount_paths, client_hosts, tokens,
                request, environment, context, client_ids,
                env_description_file):
    ids = [context.get_user(user).id for user in list_parser(users)]

    mount_users(request, environment, context, client_ids,
                env_description_file, users=list_parser(users), ids=ids,
                client_instances=list_parser(client_instances),
                mount_paths=list_parser(mount_paths),
                client_hosts=list_parser(client_hosts),
                tokens=list_parser(tokens))


@when(parsers.parse('{spaces} is mounted for {user} on {client_node}'))
@then(parsers.parse('{spaces} is mounted for {user} on {client_node}'))
@when(parsers.parse('{spaces} are mounted for {user} on {client_node}'))
@then(parsers.parse('{spaces} are mounted for {user} on {client_node}'))
def check_spaces(spaces, user, client_node, context):
    multi_auth_steps.check_spaces(spaces, user, make_arg_list(client_node),
                                  context)
