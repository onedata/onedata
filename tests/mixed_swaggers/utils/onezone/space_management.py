"""Utils and fixtures to facilitate operations in Onezone using REST API.
"""

__author__ = "Michal Cwiertnia"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


from tests.mixed_swaggers.onezone_client import (UserApi, Space)
from tests.mixed_swaggers.utils.common import login_to_oz


def create_space_using_rest(user, users, hosts, zone_name, space_name):
    user_client = login_to_oz(user, users[user].password,
                              hosts['onezone'][zone_name])
    user_api = UserApi(user_client)

    new_space = Space()
    new_space.name = space_name
    user_api.create_user_space(new_space)


def assert_item_has_appeared_in_zone_rest(user, users, hosts, zone_name,
                                          item_name):
    user_client = login_to_oz(user, users[user].password,
                              hosts['onezone'][zone_name])

    user_api = UserApi(user_client)
    user_spaces = user_api.list_user_spaces().spaces

    for sid in user_spaces:
        space = user_api.get_user_space(sid)
        if space.name == item_name:
            return True
    raise RuntimeError("No item named: {} in {} zone".format(item_name,
                                                             zone_name))
