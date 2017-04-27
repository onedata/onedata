"""Steps used in login page"""

from pytest_bdd import when, then, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser

from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed, parse_seq


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@when(parsers.re('user of (?P<browser_id_list>.+?) clicked on (?P<btn>.+?) '
                 'button in (?P<content>.+?) for "(?P<record>.+?)" sidebar '
                 'item in (?P<panel>op panel|oz panel)'))
@then(parsers.re('user of (?P<browser_id_list>.+?) clicked on (?P<btn>.+?) '
                 'button in (?P<content>.+?) for "(?P<record>.+?)" sidebar '
                 'item in (?P<panel>op panel|oz panel)'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_click_on_btn_for_record(selenium, browser_id_list, btn, content,
                               record, panel, op_panel, oz_panel):
    for browser_id in parse_seq(browser_id_list):
        cls = op_panel if panel == 'op panel' else oz_panel
        panel = cls(select_browser(selenium, browser_id))
        content = content.strip().lower().replace(' ', '_')
        btn = btn.strip().lower().replace(' ', '_')
        button = getattr(getattr(panel.sidebar.records[record], content), btn)
        button.click()
