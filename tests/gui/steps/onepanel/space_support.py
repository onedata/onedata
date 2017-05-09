"""Steps used in space support"""

from pytest_bdd import when, then, parsers

from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@when(parsers.parse('user of {browser_id} selects "{storage}" from storage '
                    'selector in support space form for "{cluster}" '
                    'in op panel'))
@then(parsers.parse('user of {browser_id} selects "{storage}" from storage '
                    'selector in support space form for "{cluster}" '
                    'in op panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_select_storage_in_support_space_form(selenium, browser_id, storage,
                                            op_panel, cluster):
    driver = selenium[browser_id]
    form = op_panel(driver).sidebar.records[cluster].spaces.add_support_form
    selector = form.storage_selector
    if not selector.is_expanded():
        selector.expand()
    selector.storages[storage].click()


@when(parsers.re('user of (?P<browser_id>.+?) clicks on (?P<btn>Support space) '
                 'button in support space form for "(?P<cluster>.+?)" '
                 'in op panel'))
@then(parsers.re('user of (?P<browser_id>.+?) clicks on (?P<btn>Support space) '
                 'button in support space form for "(?P<cluster>.+?)" '
                 'in op panel'))
@when(parsers.re('user of (?P<browser_id>.+?) selects (?P<btn>MB|GB|TB) radio '
                 'button in support space form for "(?P<cluster>.+?)" '
                 'in op panel'))
@then(parsers.re('user of (?P<browser_id>.+?) selects (?P<btn>MB|GB|TB) radio '
                 'button in support space form for "(?P<cluster>.+?)" '
                 'in op panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_click_on_btn_in_space_support_add_form(selenium, browser_id, btn,
                                              op_panel, cluster):
    driver = selenium[browser_id]
    form = op_panel(driver).sidebar.records[cluster].spaces.add_support_form
    getattr(form, btn.strip().lower().replace(' ', '_')).click()


@when(parsers.parse('user of {browser_id} types received token to Support token '
                    'field in support space form for "{cluster}" in op panel'))
@then(parsers.parse('user of {browser_id} types received token to Support token '
                    'field in support space form for "{cluster}" in op panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_received_token_to_support_token_field(selenium, browser_id,
                                                  op_panel, cluster,
                                                  tmp_memory):
    driver = selenium[browser_id]
    form = op_panel(driver).sidebar.records[cluster].spaces.add_support_form
    form.token = tmp_memory[browser_id]['mailbox']['token']


@when(parsers.parse('user of {browser_id} types "{text}" to {input_box} input '
                    'field in support space form for "{cluster}" in op panel'))
@then(parsers.parse('user of {browser_id} types "{text}" to {input_box} input '
                    'field in support space form for "{cluster}" in op panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_text_to_input_box_in_space_support_form(selenium, browser_id,
                                                    text, input_box,
                                                    op_panel, cluster):
    driver = selenium[browser_id]
    form = op_panel(driver).sidebar.records[cluster].spaces.add_support_form
    setattr(form, input_box.strip().lower().replace(' ', '_'), text)
