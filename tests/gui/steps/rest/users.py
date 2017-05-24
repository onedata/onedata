"""Steps for users management using REST API.
"""

from uuid import uuid4 as uuid
from itertools import izip
from collections import namedtuple

from tests.gui.utils.onepanel_client import UserCreateRequest
from tests.gui.utils.onepanel_client.configuration import Configuration as OnepanelConf
from tests.gui.utils.onepanel_client.rest import ApiException

from pytest_bdd import given, parsers
from tests.gui.utils.generic import parse_seq

from pytest import skip

from .common import get_panel_client


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


PASSWORD = 'password'

UserCred = namedtuple('UserCredentials', ['username', 'password', 'id'])
OnepanelConf().verify_ssl = False

_used_usernames = set()


def _create_user(api_client, username, password):
    try:
        api_client.get_user(username)
    except ApiException as ex:
        if ex.status == 404:
            api_client.add_user(UserCreateRequest(username=username,
                                                  password=password,
                                                  user_role='regular'))
            return api_client.get_user(username).user_id
        else:
            skip('failed to create "{}" user'.format(username))
    else:
        skip('"{}" user already exist'.format(username))


@given(parsers.parse('user of {browser_id_list} has account '
                     'in "{host}" Onezone service'))
@given(parsers.parse('users of {browser_id_list} have accounts '
                     'in "{host}" Onezone service'))
def g_create_unique_users(browser_id_list, host, admin_credentials, hosts, users):
    global _used_usernames

    api_client = get_panel_client(admin_credentials, hosts['zone_panel'][host])
    _users = {}
    for browser_id, username in izip(parse_seq(browser_id_list),
                                     iter(username for username
                                          in iter(lambda: uuid().hex[:6], 'a')
                                          if username not in _used_usernames)):
        user_id = _create_user(api_client, username, 'password')
        users[browser_id] = _users[browser_id] = UserCred(username=username,
                                                          password=PASSWORD,
                                                          id=user_id)
        _used_usernames.add(username)

    yield

    api_client = get_panel_client(admin_credentials, hosts['zone_panel'][host])
    for credential in _users.values():
        api_client.remove_user(credential.username)


@given(parsers.re(r'user of (?P<browser_id_list>.+?) has account '
                  r'in "(?P<host>.?)" Onezone service with '
                  r'(?P<credentials_list>.+?) credentials'))
@given(parsers.re(r'users of (?P<browser_id_list>.+?) have account '
                  r'in "(?P<host>.?)" Onezone service with '
                  r'(?P<credentials_list>.+?) credentials'))
def g_create_users_with_given_credentials(browser_id_list, credentials_list,
                                          host, admin_credentials, hosts, users):
    global _used_usernames

    api_client = get_panel_client(admin_credentials, hosts['zone_panel'][host])
    _users = {}
    for browser_id, credential in izip(parse_seq(browser_id_list),
                                       parse_seq(credentials_list)):
        username, password = credential.split(':')
        user_id = _create_user(api_client, username, password)
        users[browser_id] = _users[browser_id] = UserCred(username=username,
                                                          password=password,
                                                          id=user_id)
        _used_usernames.add(username)

    yield

    api_client = get_panel_client(admin_credentials, hosts['zone_panel'][host])
    for credential in _users.values():
        api_client.remove_user(credential.username)


# @then(parsers.parse('user\'s of {browser_id} alias has changed to his unique username in database'))
# def check_if_users_alias_has_changed_to_his_unique_username_in_database(browser_id, tmp_memory,
#                                                                         onezone_rest_url):
#     Conf_OZ().verify_ssl = False
#     username = tmp_memory[browser_id]['username']
#     user_client = login_to_oz_with_given_username_and_password(username,
#                                                                'Password1',
#                                                                onezone_rest_url)
#     user_api = UserApi(user_client)
#     user = user_api.get_user()
#     assert user.alias == username
#
#
# @given('first provider name is known')
# def first_provider_name(admin_credentials, op_panel_url):
#     admin_client = login_to_op_with_given_username_and_password(admin_credentials['username'],
#                                                                 admin_credentials['password'],
#                                                                 op_panel_url)
#
#     provider_api = OneproviderApi(admin_client)
#     provider = provider_api.get_provider().name
#     return provider
