"""Steps for users creation using REST API.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from collections import namedtuple

from tests.gui.utils.onepanel_client import UserCreateRequest
from tests.gui.utils.onepanel_client.rest import ApiException

from pytest_bdd import given, parsers
from tests.gui.utils.generic import parse_seq, suppress

from pytest import skip

from ..common import get_panel_api, get_oz_user_api


UserCred = namedtuple('UserCredentials', ['username', 'password', 'id'])


@given(parsers.re('there (is|are) (?P<users_list>.*?) in '
                  '"(?P<host>.*?)" Onezone service'))
def users(users_list, host, admin_credentials, hosts):
    admin_client = get_panel_api(admin_credentials.username,
                                 admin_credentials.password,
                                 hosts['zone_panel'][host])

    users_info = {username: _create_user(admin_client, username,
                                         'password', 'regular',
                                         hosts['onezone'][host])
                  for username in parse_seq(users_list)}

    yield users_info

    # after test is done, remove created users
    for user in users_info.values():
        with suppress(ApiException):
            admin_client.remove_user(user.username)


def _create_user(admin_client, username, password, user_role, zone_host):
    # assert user does not exist, otherwise skip
    try:
        admin_client.get_user(username)
    except ApiException as ex:
        if ex.status == 404:
            admin_client.add_user(UserCreateRequest(username=username,
                                                    password=password,
                                                    user_role=user_role))

            # user is created in zone panel and not zone itself
            # so for them to be created also in zone
            # login/rest call to zone using his credentials must be made
            # only then zone will ask if this user exist and if so remember it
            zone_client = get_oz_user_api(username, password, zone_host)
            user_id = zone_client.get_current_user().user_id
            return UserCred(username=username, password=password, id=user_id)
        else:
            skip('failed to create "{}" user'.format(username))
    else:
        skip('"{}" user already exist'.format(username))
