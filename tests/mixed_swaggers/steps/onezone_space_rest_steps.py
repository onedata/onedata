"""This module contains gherkin steps to run acceptance tests featuring
basic operations on spaces in Onezone using REST API.
"""

__author__ = "Michal Cwiertnia"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


from tests.mixed_swaggers.onezone_client.configuration import (Configuration
                                                               as Conf_OZ)
from tests.mixed_swaggers.onezone_client import (ApiClient as ApiClient_OZ,
                                                 UserApi, Space)
from tests import OZ_REST_PATH_PREFIX, OZ_REST_PORT


def _login_to_oz(username, password, host):
    Conf_OZ().verify_ssl = False
    Conf_OZ().username = username
    Conf_OZ().password = password

    client = ApiClient_OZ(host="https://{}:{}{}".format(
        host,
        OZ_REST_PORT,
        OZ_REST_PATH_PREFIX),
        header_name='authorization',
        header_value=Conf_OZ().get_basic_auth_token())

    return client


def create_space_using_rest(user, users, hosts, zone_name, space_name):
    user_client = _login_to_oz(user, users[user].password,
                               hosts['onezone'][zone_name])
    user_api = UserApi(user_client)

    new_space = Space()
    new_space.name = space_name
    user_api.create_user_space(new_space)


def assert_item_has_appeared_in_zone_rest(user, users, hosts, zone_name,
                                          item_name):
    user_client = _login_to_oz(user, users[user].password,
                               hosts['onezone'][zone_name])

    user_api = UserApi(user_client)
    user_spaces = user_api.get_user_spaces().spaces

    for sid in user_spaces:
        space = user_api.get_user_space(sid)
        if space.name == item_name:
            return True
    assert False
