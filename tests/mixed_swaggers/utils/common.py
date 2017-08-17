"""Utils and fixtures to facilitate common operations using REST API.
"""

__author__ = "Michal Cwiertnia"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


from tests.mixed_swaggers.onezone_client.configuration import (Configuration
                                                               as Conf_OZ)
from tests.mixed_swaggers.onezone_client import (ApiClient as ApiClient_OZ)
from tests import OZ_REST_PATH_PREFIX, OZ_REST_PORT


class NoSuchClientException(Exception):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return repr(self.value)


def login_to_oz(username, password, host):
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
