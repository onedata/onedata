"""This module contains gherkin steps to run acceptance tests featuring
basic operations on spaces in Onezone using REST API mixed with web GUI.
"""

__author__ = "Michal Cwiertnia"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


from pytest_bdd import when, then
from tests.gui.gui_meta_steps.onezone import *

from tests.mixed_swaggers.utils.common import *
from tests.mixed_swaggers.utils.onezone.space_management import *


@when(parsers.re(r'using <(?P<client>client\d)>, (?P<user>.+?) creates space '
                 '"(?P<space_name>.+?)" in "(?P<zone_name>.+?)" Onezone '
                 'service'))
def create_space_in_oz(client, client1, client2, user, space_name, zone_name,
                       hosts, users, selenium, oz_page):
    client = locals()[client].lower()
    if client == 'rest':
        create_space_in_oz_using_rest(user, users, hosts, zone_name, space_name)
    elif client == 'onezone web gui':
        create_space_in_oz_using_gui(selenium, user, oz_page, space_name)
    else:
        raise NoSuchClientException("Client: {} not found.".format(client))


@then(parsers.re(r'using <(?P<client>client\d)>, (?P<user>.+?) sees that '
                 '(?P<item_type>.+?) named "(?P<item_name>.+?)" has appeared '
                 'in "(?P<zone_name>.+?)" Onezone service'))
def assert_there_is_item_in_oz(client, client1, client2, user, item_type,
                               item_name, selenium, oz_page, users, hosts,
                               zone_name):
    client = locals()[client].lower()
    if client == 'rest':
        assert_item_has_appeared_in_oz_rest(user, users, hosts, zone_name,
                                            item_name)
    elif client == 'onezone web gui':
        assert_item_has_appeared_in_oz_gui(selenium, user, oz_page,
                                           item_type, item_name)
    else:
        raise NoSuchClientException("Client: {} not found.".format(client))
