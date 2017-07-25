"""Module implements pytest-bdd steps for user management via REST
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
from tests.utils.acceptance_utils import list_parser
from tests.utils.utils import set_dns
from tests.utils.user_utils import create_user, delete_user, get_id
from tests.utils.space_utils import delete_space

from environment import docker

from pytest_bdd import given, parsers


@given(parsers.re("users (?P<users>.*) register with passwords (?P<passwords>.*)"))
def register_users(users, passwords, context, onedata_environment, request):
    set_dns(onedata_environment)
    users = list_parser(users)
    passwords = list_parser(passwords)
    onepanel = onedata_environment['onepanel_nodes'][0].split('@')[1]

    if not hasattr(context, "users"):
        context.users = {}

    for user_name, password in zip(users, passwords):
        user = create_user(user_name, password, onepanel)
        user.set_oz_domain(onedata_environment)
        context.users[user_name] = user

    def fin():
        for user_name in users:
            user = context.get_user(user_name)
            for space in user.created_spaces.keys():
                delete_space(user, space)
            delete_user(user_name, onepanel)
            del context.users[user_name]

    request.addfinalizer(fin)
    return users


@given(parsers.re('users (?P<users>.*) authorize with (?P<provider_ids>.*) certs'))
def provider_certs(users, provider_ids, context, providers):

    users = context.get_users(list_parser(users))
    provider_ids = list_parser(provider_ids)

    for user, provider_id in zip(users, provider_ids):
        user.provider = providers[provider_id]
        user.get_certs_from_provider()


@given(parsers.re('users (?P<users>.*) get their ids from OZ via REST'))
def user_ids(users, context):
    users = context.get_users(list_parser(users))

    for user in users:
        user_id = get_id(user)
        user.id = user_id
