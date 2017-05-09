"""Steps used in login page"""

from pytest_bdd import when, then, parsers

from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed, parse_seq


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@when(parsers.re('users? of (?P<browser_id_list>.+?) clicks? on (?P<btn>.+?) '
                 'button in (?P<content>.+?) for "(?P<record>.+?)" sidebar '
                 'item in (?P<panel>op|oz) panel'))
@then(parsers.re('users? of (?P<browser_id_list>.+?) clicks? on (?P<btn>.+?) '
                 'button in (?P<content>.+?) for "(?P<record>.+?)" sidebar '
                 'item in (?P<panel>op|oz) panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_click_on_btn_for_record(selenium, browser_id_list, btn, content,
                               record, panel, op_panel, oz_panel):
    for browser_id in parse_seq(browser_id_list):
        cls = op_panel if panel == 'op' else oz_panel
        panel = cls(selenium[browser_id])
        content = content.strip().lower().replace(' ', '_')
        btn = btn.strip().lower().replace(' ', '_')
        button = getattr(getattr(panel.sidebar.records[record], content), btn)
        button.click()


@when(parsers.re('users? of (?P<browser_id_list>.+?) clicks? on '
                 '(?P<sub_item>.+?) item in submenu of "(?P<record>.+?)" '
                 'item in (?P<panel>op|oz) panel'))
@then(parsers.re('users? of (?P<browser_id_list>.+?) clicks? on '
                 '(?P<sub_item>.+?) item in submenu of "(?P<record>.+?)" '
                 'item in (?P<panel>op|oz) panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_click_on_subitem_for_record(selenium, browser_id_list, sub_item,
                                   record, panel, op_panel, oz_panel):
    for browser_id in parse_seq(browser_id_list):
        cls = op_panel if panel == 'op' else oz_panel
        panel = cls(selenium[browser_id])
        panel.sidebar.records[record].submenu[sub_item].click()
