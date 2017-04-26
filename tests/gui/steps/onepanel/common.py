"""Steps used in login page"""

from itertools import izip

from pytest_bdd import given, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser

from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed, parse_seq


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@given(parsers.parse('user of {browser_id_list} clicked on {btn} button '
                     'in content page for "{record}" sidebar record'))
@given(parsers.parse('users of {browser_id_list} clicked on {btn} button '
                     'in content page for "{record}" sidebar record'))
@repeat_failed(timeout=WAIT_FRONTEND)
def g_click_on_btn_for_record(selenium, browser_id_list,
                              users_list, users, login_page, op_panel, oz_panel):
    for browser_id, username in izip(parse_seq(browser_id_list),
                                     parse_seq(users_list)):
        login = login_page(select_browser(selenium, browser_id))
        login.username = username
        login.password = users[username]
