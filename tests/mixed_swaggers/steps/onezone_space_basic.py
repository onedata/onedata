"""This module contains gherkin steps to run acceptance tests featuring
basic operations on spaces in Onezone using REST API mixed with web GUI.
"""

__author__ = "Michal Cwiertnia"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")

from pytest_bdd import when, given, then, parsers

from tests.gui.gui_meta_steps.onezone import *

from tests.mixed_swaggers.steps.onezone_space_rest_steps import *


@when(parsers.parse('using <client1>, {user} creates space "{space_name}"'
                    ' in "{zone_name}" Onezone service'))
def create_space(client1, user, space_name, zone_name, hosts, users, selenium,
                 oz_page):
    if client1.lower() == "rest":
        create_space_using_rest(user, users, hosts, zone_name, space_name)

    if client1.lower() == "web gui":
        create_space_using_gui(selenium, user, oz_page, space_name)


@then(parsers.parse('using <client2>, {user} sees that {item_type} '
                    'named "{item_name}" has appeared in "{zone_name}" Onezone '
                    'service'))
def assert_there_is_item_in_zone(client2, user, item_type, item_name, selenium,
                                 oz_page, users, hosts, zone_name):
    if client2.lower() == "web gui":
        assert_item_has_appeared_in_zone_gui(selenium, user, oz_page,
                                              item_type, item_name)

    if client2.lower() == "rest":
        assert_item_has_appeared_in_zone_rest(user, users, hosts, zone_name,
                                              item_name)

