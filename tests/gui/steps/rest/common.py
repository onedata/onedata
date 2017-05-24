"""Steps for users management using REST API.
"""

from enum import Enum

from tests.gui.utils.onepanel_client.configuration import Configuration as OnepanelConf
from tests.gui.utils.onepanel_client import (ApiClient as OnepanelClient,
                                             OnezoneApi as OzPanelApi,
                                             OneproviderApi as OpPanelApi,
                                             OnepanelApi)

from tests.gui.utils.onezone_client.configuration import Configuration as OnezoneConf
from tests.gui.utils.onezone_client import (ApiClient as OnezoneClient,
                                            UserApi as OnezoneUserApi)


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Service(Enum):
    ONEPANEL = 1
    OP_PANEL = 2
    OZ_PANEL = 3
    ONEZONE = 4
    ONEPROVIDER = 5

a = {
    Service.ONEPANEL: (OnepanelConf(), OnepanelClient, OnepanelApi,
                       'https://{}/api/v3/onepanel'),
    Service.OZ_PANEL: (OnepanelConf(), OnepanelClient, OzPanelApi,
                       'https://{}/api/v3/onepanel'),
    Service.OP_PANEL: (OnepanelConf(), OnepanelClient, OpPanelApi,
                       'https://{}/api/v3/onepanel'),
    Service.ONEZONE: (OnezoneConf(), OnezoneClient, OnezoneUserApi,
                      'https://{}/api/v3/onezone')
}


def get_panel_client(admin_credentials, host):
    return get_rest_client(admin_credentials, host, Service.ONEPANEL)


def get_op_panel_client(admin_credentials, host):
    return get_rest_client(admin_credentials, host, Service.OP_PANEL)


def get_oz_panel_client(admin_credentials, host):
    return get_rest_client(admin_credentials, host, Service.OZ_PANEL)


def get_zone_client(user_credentials, host):
    return get_rest_client(user_credentials, host, Service.ONEZONE)


def get_rest_client(credentials, host, service):
    config, client, api, endpoint_template = a[service]
    config.username = credentials.username
    config.password = credentials.password
    return api(client(host=endpoint_template.format(host)))
