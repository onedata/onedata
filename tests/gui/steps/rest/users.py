"""Steps for users management using REST API.
"""

from uuid import uuid4 as uuid
from itertools import izip
from collections import namedtuple

from tests.gui.utils.onepanel_client import UserCreateRequest
from tests.gui.utils.onepanel_client.rest import ApiException

from pytest_bdd import given, parsers
from tests.gui.utils.generic import parse_seq

from pytest import skip, fixture

from .common import get_panel_api, get_oz_user_api


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


UserCred = namedtuple('UserCredentials', ['username', 'password', 'id'])


@fixture(scope='session')
def available_usernames():
    used_usernames = {'aaaaaaaa'}

    def _next_available_username():
        name = 'aaaaaaaa'
        while True:
            while name in used_usernames:
                name = uuid().hex[:8]

            used_usernames.add(name)
            yield name

    return _next_available_username()


def _create_user(api_client, username, password, zone_host):
    try:
        api_client.get_user(username)
    except ApiException as ex:
        if ex.status == 404:
            api_client.add_user(UserCreateRequest(username=username,
                                                  password=password,
                                                  user_role='regular'))

            # user is created in zone panel and not zone itself
            # so for them to be created also in zone
            # login/rest call to zone using his credentials must be made
            # only then zone will ask if this user exist and if so remember it
            zone_client = get_oz_user_api(username, password, zone_host)
            return zone_client.get_current_user().user_id

        else:
            skip('failed to create "{}" user'.format(username))
    else:
        skip('"{}" user already exist'.format(username))


@given(parsers.parse('user of {browser_id_list} has account '
                     'in "{host}" Onezone service'))
@given(parsers.parse('users of {browser_id_list} have accounts '
                     'in "{host}" Onezone service'))
def g_create_unique_users(browser_id_list, host, admin_credentials,
                          hosts, users, available_usernames):
    client = get_panel_api(admin_credentials.username,
                           admin_credentials.password,
                           hosts['zone_panel'][host])
    password = 'password'
    for browser_id, username in izip(parse_seq(browser_id_list),
                                     available_usernames):
        user_id = _create_user(client, username, password, hosts['onezone'][host])
        users[browser_id] = UserCred(username=username, password=password,
                                     id=user_id)
