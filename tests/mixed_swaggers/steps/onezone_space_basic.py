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


@when(parsers.parse('using <client1>, {user} creates space "{space_name}"'
                    ' in "{zone_name}" Onezone service'))
def create_space(client1, user, space_name, zone_name, hosts, users, selenium,
                 oz_page):
    client1 = client1.lower()
    if client1 == 'rest':
        create_space_using_rest(user, users, hosts, zone_name, space_name)
    elif client1 == 'onezone web gui':
        create_space_using_gui(selenium, user, oz_page, space_name)
    else:
        raise NoSuchClientException("Client: {} not found.".format(client1))


@then(parsers.parse('using <client2>, {user} sees that {item_type} '
                    'named "{item_name}" has appeared in "{zone_name}" Onezone '
                    'service'))
def assert_there_is_item_in_zone(client2, user, item_type, item_name, selenium,
                                 oz_page, users, hosts, zone_name):
    client2 = client2.lower()
    if client2 == 'rest':
        assert_item_has_appeared_in_zone_rest(user, users, hosts, zone_name,
                                              item_name)
    elif client2 == 'onezone web gui':
        assert_item_has_appeared_in_zone_gui(selenium, user, oz_page,
                                             item_type, item_name)
    else:
        raise NoSuchClientException("Client: {} not found.".format(client2))
