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
from tests.gui.utils.onepanel_client.rest import ApiException as OnepanelApiException

from pytest_bdd import given, parsers
from tests.gui.utils.generic import suppress

from pytest import skip

from ..common import get_panel_api, get_oz_user_api


UserCred = namedtuple('UserCredentials', ['username', 'password', 'id'])


@given(parsers.parse('initial users configuration in "{host}" '
                     'Onezone service:\n{config}'))
def users(host, config, admin_credentials, hosts):
    zone_hostname = hosts['onezone'][host]
    admin_client = _get_admin_client(admin_credentials.username,
                                     admin_credentials.password,
                                     host, hosts)

    users_info = {}
    for user in yaml.load(config):
        try:
            [(user, options)] = user.items()
        except AttributeError:
            options = {}

        try:
            user_info = _create_user(admin_client, user, options,
                                     hosts['onezone'][host])
        except Exception as ex:
            _rm_users(admin_client, users_info, zone_hostname)
            raise ex
        else:
            users_info[user] = user_info

    yield users_info

    # after test is done, remove created users
    _rm_users(admin_client, users_info, zone_hostname)


def _get_admin_client(username, password, host, hosts):
    # make call to oz to create admin user in onezone
    zone_client = get_oz_user_api(username, password, hosts['onezone'][host])
    zone_client.get_current_user()
    return get_panel_api(username, password, hosts['zone_panel'][host])


def _create_user(admin_panel_api, username, options, zone_hostname):
    password = options.get('password', 'password')
    user_role = options.get('user role', 'regular')
    alias = options.get('alias', None)

    # assert user does not exist, otherwise skip
    try:
        admin_panel_api.get_user(username)
    except OnepanelApiException as ex:
        if ex.status == 404:
            admin_panel_api.add_user(UserCreateRequest(username=username,
                                                       password=password,
                                                       user_role=user_role))

            # user is created in zone panel and not zone itself
            # so for them to be created also in zone
            # login/rest call to zone using his credentials must be made,
            # then zone will ask panel for given user and store given info
            user_zone_api = get_oz_user_api(username, password, zone_hostname)
            user_id = user_zone_api.get_current_user().user_id
            if alias:
                update_user_request = UserUpdateRequest(alias=alias)
                try:
                    user_zone_api.modify_current_user(update_user_request)
                except OzApiException:
                    _rm_user(admin_panel_api, username,
                             password, zone_hostname)
                    skip('"{}" alias is already occupied '
                         'by other user'.format(alias))
            return UserCred(username=username, password=password, id=user_id)
        else:
            skip('failed to create "{}" user'.format(username))
    else:
        skip('"{}" user already exist'.format(username))


def _rm_users(admin_panel_client, users_credentials, zone_hostname):
    for user in users_credentials.values():
        _rm_user(admin_panel_client, user.username,
                 user.password, zone_hostname)


def _rm_user(admin_panel_api, username, password, zone_hostname):
    user_zone_api = get_oz_user_api(username, password, zone_hostname)
    with suppress(OnepanelApiException, OzApiException):
        user_zone_api.remove_current_user()
        admin_panel_api.remove_user(username)
