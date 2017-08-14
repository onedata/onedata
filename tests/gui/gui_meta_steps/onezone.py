"""This module contains meta steps for operations in Onezone using web GUI
"""

__author__ = "Michal Cwiertnia"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


from pytest_bdd import given, parsers

from tests.gui.steps.onezone.\
    logged_in_common import (wt_expand_oz_panel,
                             assert_there_is_item_named_in_oz_panel_list,
                             click_on_btn_in_oz_panel)
from tests.gui.steps.onezone.data_space_management import \
    type_text_into_space_creation_edit_box_in_oz
from tests.gui.steps.common.miscellaneous import press_enter_on_active_element
from tests.gui.steps.common.browser_creation import \
    create_instances_of_webdriver
from tests.gui.steps.common.url import g_open_onedata_service_page, refresh_site
from tests.gui.steps.onezone.login_page import g_login_to_zone_using_basic_auth


def create_space_in_oz_using_gui(selenium, user, oz_page, space_name):
    panel_name = "DATA SPACE MANAGEMENT"
    button_name = "Create new space"
    item_type = "space"
    wt_expand_oz_panel(selenium, user, panel_name, oz_page)
    click_on_btn_in_oz_panel(selenium, user, button_name, panel_name, oz_page)
    type_text_into_space_creation_edit_box_in_oz(selenium, user,
                                                 space_name, oz_page)
    press_enter_on_active_element(selenium, user)
    assert_there_is_item_named_in_oz_panel_list(selenium, user, item_type,
                                                space_name, panel_name,
                                                oz_page)


def assert_item_has_appeared_in_oz_gui(selenium, user, oz_page, item_type,
                                       item_name):
    panel_name = "DATA SPACE MANAGEMENT"
    refresh_site(selenium, user)
    wt_expand_oz_panel(selenium, user, panel_name, oz_page)
    assert_there_is_item_named_in_oz_panel_list(selenium, user, item_type,
                                                item_name, panel_name,
                                                oz_page)


@given(parsers.parse('logged as {user} to "{host_name}" service using web GUI'))
def login_to_oz_using_gui(user, host_name, selenium, driver, tmpdir, tmp_memory,
                          driver_kwargs, driver_type, firefox_logging,
                          firefox_path, xvfb, screen_width, screen_height,
                          displays, hosts, users, oz_login_page):
    create_instances_of_webdriver(selenium, driver, user, tmpdir,
                                  tmp_memory, driver_kwargs, driver_type,
                                  firefox_logging, firefox_path, xvfb,
                                  screen_width, screen_height, displays)
    g_open_onedata_service_page(selenium, user, host_name, hosts)
    g_login_to_zone_using_basic_auth(selenium, user, user,
                                     oz_login_page, users)
