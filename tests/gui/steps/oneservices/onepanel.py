"""Steps for onepanel REST API usage.
"""

from uuid import uuid4 as uuid
from itertools import izip
from collections import namedtuple

from tests.gui.utils.onepanel_client import (UserCreateRequest, OnepanelApi,
                                             ApiClient as OnepanelClient)
from tests.gui.utils.onepanel_client.configuration import Configuration as OnepanelConf
from tests.gui.utils.onepanel_client.rest import ApiException

from pytest_bdd import given, parsers
from tests.gui.utils.generic import parse_seq

from pytest import skip


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


UserCred = namedtuple('UserCredentials', ['username', 'password'])
OnepanelConf().verify_ssl = False
_created_users = set()


def _heh(admin_credentials, host):
    config = OnepanelConf()
    config.username = admin_credentials.username
    config.password = admin_credentials.password
    host = 'https://{}/api/v3/onepanel'.format(host)
    return OnepanelApi(OnepanelClient(host=host))


def _create_user(api_client, user_cred):
    try:
        api_client.get_user(user_cred.username)
    except ApiException as ex:
        if ex.status == 404:
            api_client.add_user(UserCreateRequest(username=user_cred.username,
                                                  password=user_cred.password,
                                                  user_role='regular'))
        else:
            skip('failed to create "{}" user'.format(user_cred.username))
    else:
        skip('"{}" user already exist'.format(user_cred.username))


@given(parsers.parse('user of {browser_id_list} has account '
                     'in "{host}" Onezone service'))
@given(parsers.parse('users of {browser_id_list} have accounts '
                     'in "{host}" Onezone service'))
def g_create_unique_users(browser_id_list, host, admin_credentials, hosts, users):
    global _created_users

    api_client = _heh(admin_credentials, hosts['zone_panel'][host])
    _users = {}
    for browser_id, username in izip(parse_seq(browser_id_list),
                                     iter(username for username
                                          in iter(lambda: uuid().hex[:6], 'a')
                                          if username not in _created_users)):
        users[browser_id] = _users[browser_id] = UserCred(username=username,
                                                          password='password')
        _create_user(api_client, _users[browser_id])
        _created_users.add(username)

    yield

    api_client = _heh(admin_credentials, hosts['zone_panel'][host])
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
    global _created_users

    api_client = _heh(admin_credentials, hosts['zone_panel'][host])
    _users = {}
    for browser_id, credential in izip(parse_seq(browser_id_list),
                                       parse_seq(credentials_list)):
        username, password = credential.split(':')
        users[browser_id] = _users[browser_id] = UserCred(username=username,
                                                          password=password)
        _create_user(api_client, _users[browser_id])
        _created_users.add(username)

    yield

    api_client = _heh(admin_credentials, hosts['zone_panel'][host])
    for credential in _users.values():
        api_client.remove_user(credential.username)


# def login_to_oz_with_given_username_and_password(username, password, host):
#     USERNAME = username
#     PASSWORD = password
#     Conf_OZ().username = USERNAME
#     Conf_OZ().password = PASSWORD
#
#     client = ApiClient_OZ(host=host,
#                           header_name='authorization',
#                           header_value=Conf_OZ().get_basic_auth_token())
#     return client
#
#
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
