"""This module contains gherkin steps to run acceptance tests featuring
login page in onezone web GUI.
"""

__author__ = "Michal Cwiertnia"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")

from pytest_bdd import when, parsers
from tests.mixed_swaggers.onezone_client.configuration import (Configuration
                                                               as Conf_OZ)
from tests.mixed_swaggers.onezone_client import (ApiClient as ApiClient_OZ,
                                                 UserApi, Space)
from tests import OZ_REST_PATH_PREFIX, OZ_REST_PORT


@when(parsers.parse('"{username}" creates space "{space_name}"'))
def create_spaces(username, space_name, hosts, users):

        Conf_OZ().verify_ssl = False
        Conf_OZ().username = username
        Conf_OZ().password = users[username].password

        user_client = ApiClient_OZ(host="https://{}:{}{}".
                                   format(hosts['onezone']['z1'],
                                          OZ_REST_PORT,
                                          OZ_REST_PATH_PREFIX),
                                   header_name='authorization',
                                   header_value=Conf_OZ().
                                   get_basic_auth_token())

        user_api = UserApi(user_client)

        new_space = Space()
        new_space.name = space_name
        user_api.create_user_space(new_space)
