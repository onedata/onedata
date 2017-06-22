"""Steps for users creation using REST API.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import yaml
from collections import namedtuple

from tests.gui.utils.onezone_client import UserUpdateRequest
from tests.gui.utils.onezone_client.rest import ApiException as OzApiException
from tests.gui.utils.onepanel_client import UserCreateRequest
from tests.gui.utils.onepanel_client.rest import ApiException

from pytest_bdd import given, parsers
from tests.gui.utils.generic import suppress

from pytest import skip

from ..common import get_panel_api, get_oz_user_api


UserCred = namedtuple('UserCredentials', ['username', 'password', 'id'])


@given(parsers.parse('initial users configuration in "{host}" '
                     'Onezone service:\n{config}'))
def users(host, config, admin_credentials, hosts):
    zone_host = hosts['onezone'][host]
    admin_client = _get_admin_client(admin_credentials.username,
                                     admin_credentials.password,
                                     host, hosts)

    users_info = {}
    for user in yaml.load(config):
        try:
            [(user, options)] = user.items()
        except AttributeError:
            options = {}

        users_info[user] = _create_user(admin_client, user, options,
                                        hosts['onezone'][host])

    yield users_info

    # after test is done, remove created users
    for user in users_info.values():
        zone_client = get_oz_user_api(user.username, user.password,
                                      zone_host)
        with suppress(ApiException, OzApiException):
            zone_client.remove_current_user()
            admin_client.remove_user(user.username)


def _create_user(admin_client, username, options, zone_host):
    password = options.get('password', 'password')
    user_role = options.get('user role', 'regular')
    alias = options.get('alias', None)
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
            if alias:
                request = UserUpdateRequest(alias=alias)
                try:
                    zone_client.modify_current_user(request)
                except OzApiException:
                    zone_client.remove_current_user()
                    admin_client.remove_user(username)
                    skip('"{}" alias is already occupied'.format(alias))
            return UserCred(username=username, password=password, id=user_id)
        else:
            skip('failed to create "{}" user'.format(username))
    else:
        skip('"{}" user already exist'.format(username))


def _get_admin_client(username, password, host, hosts):
    # make call to oz to create admin user in onezone
    zone_client = get_oz_user_api(username, password, hosts['onezone'][host])
    zone_client.get_current_user()
    return get_panel_api(username, password, hosts['zone_panel'][host])
