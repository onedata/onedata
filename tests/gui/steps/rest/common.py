"""Steps for users management using REST API.
"""

from enum import Enum

from tests.gui.utils.onepanel_client.configuration import Configuration as OnepanelConf
from tests.gui.utils.onepanel_client import ApiClient as OnepanelClient, OnepanelApi

from tests.gui.utils.onezone_client.configuration import Configuration as OnezoneConf
from tests.gui.utils.onezone_client import (ApiClient as OnezoneClient,
                                            UserApi as OzUserApi,
                                            GroupApi as OzGroupApi)


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


OnezoneConf().verify_ssl = False
OnepanelConf().verify_ssl = False


class Service(Enum):
    ONEPANEL = 1
    OP_PANEL = 2
    OZ_PANEL = 3
    ONEZONE = 4
    ONEPROVIDER = 5


class Api(object):
    def __init__(self, api, config, username, password):
        self.api = api
        self.config = config
        self.username = username
        self.password = password

    def __getattr__(self, item):
        attr = getattr(self.api, item)
        self.config.username = self.username
        self.config.password = self.password
        return attr


SERVICES_DETAILS = {
    Service.ONEPANEL: (OnepanelConf(), OnepanelClient,
                       'https://{}/api/v3/onepanel'),
    Service.ONEZONE: (OnezoneConf(), OnezoneClient,
                      'https://{}/api/v3/onezone')
}


def get_panel_api(username, password, host):
    return get_rest_api_client(username, password, host,
                               Service.ONEPANEL, OnepanelApi)


def get_oz_group_api(username, password, host):
    return get_rest_api_client(username, password, host,
                               Service.ONEZONE, OzGroupApi)


def get_oz_user_api(username, password, host):
    return get_rest_api_client(username, password, host,
                               Service.ONEZONE, OzUserApi)


def get_rest_api_client(username, password, host, service, api_cls):
    config, client, endpoint_template = SERVICES_DETAILS[service]
    config.username = username
    config.password = password
    return Api(api_cls(client(host=endpoint_template.format(host))),
               config, username, password)
