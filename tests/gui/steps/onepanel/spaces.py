"""Steps used in space support"""

import yaml

from pytest_bdd import when, then, parsers

from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND
from tests.gui.utils.generic import repeat_failed, transform


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@when(parsers.parse('user of {browser_id} selects "{storage}" from storage '
                    'selector in support space form in Onepanel'))
@then(parsers.parse('user of {browser_id} selects "{storage}" from storage '
                    'selector in support space form in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_select_storage_in_support_space_form(selenium, browser_id,
                                            storage, onepanel):
    storage_selector = (onepanel(selenium[browser_id]).content.spaces.
                        form.storage_selector)
    storage_selector.expand()
    storage_selector.options[storage].click()


@when(parsers.parse('user of {browser_id} clicks on Support space button '
                    'in spaces page in Onepanel if there are some spaces '
                    'already supported'))
@then(parsers.parse('user of {browser_id} clicks on Support space button '
                    'in spaces page in Onepanel if there are some spaces '
                    'already supported'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_click_on_support_space_btn_on_condition(selenium, browser_id, onepanel):
    spaces_page = onepanel(selenium[browser_id]).content.spaces
    if spaces_page.spaces.count() > 0:
        spaces_page.support_space()


@when(parsers.re('user of (?P<browser_id>.+?) selects (?P<btn>MB|GB|TB) radio '
                 'button in support space form in Onepanel'))
@then(parsers.re('user of (?P<browser_id>.+?) selects (?P<btn>MB|GB|TB) radio '
                 'button in support space form in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_select_unit_in_space_support_add_form(selenium, browser_id,
                                             btn, onepanel):
    onepanel(selenium[browser_id]).content.spaces.form.units[btn].click()


@when(parsers.re('user of (?P<browser_id>.+?) clicks on Support space '
                 'button in support space form in Onepanel'))
@then(parsers.re('user of (?P<browser_id>.+?) clicks on Support space '
                 'button in support space form in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_click_on_btn_in_space_support_add_form(selenium, browser_id, onepanel):
    onepanel(selenium[browser_id]).content.spaces.form.support_space()


@when(parsers.parse('user of {browser_id} types received token to Support token '
                    'field in support space form in Onepanel'))
@then(parsers.parse('user of {browser_id} types received token to Support token '
                    'field in support space form in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_received_token_to_support_token_field(selenium, browser_id,
                                                  onepanel, tmp_memory):
    form = onepanel(selenium[browser_id]).content.spaces.form
    form.token = tmp_memory[browser_id]['mailbox']['token']


@when(parsers.parse('user of {browser_id} types "{text}" to {input_box} input '
                    'field in support space form in Onepanel'))
@then(parsers.parse('user of {browser_id} types "{text}" to {input_box} input '
                    'field in support space form in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_text_to_input_box_in_space_support_form(selenium, browser_id, text,
                                                    input_box, onepanel):
    form = onepanel(selenium[browser_id]).content.spaces.form
    setattr(form, transform(input_box), text)


@when(parsers.re(r'user of (?P<browser_id>.*?) enables '
                 r'(?P<toggle>Mount in root|Import storage data) option '
                 r'in support space form in Onepanel'))
@then(parsers.re(r'user of (?P<browser_id>.*?) enables '
                 r'(?P<toggle>Mount in root|Import storage data) option '
                 r'in support space form in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_enable_option_box_in_space_support_form(selenium, browser_id,
                                               toggle, onepanel):
    form = onepanel(selenium[browser_id]).content.spaces.form
    getattr(form, transform(toggle)).check()


@when(parsers.parse('user of {browser_id} sees that space support record for '
                    '"{space}" has appeared in Spaces page in Onepanel'))
@then(parsers.parse('user of {browser_id} sees that space support record for '
                    '"{space}" has appeared in Spaces page in Onepanel'))
@repeat_failed(timeout=WAIT_BACKEND)
def wt_assert_existence_of_space_support_record(selenium, browser_id,
                                                space, onepanel):
    _ = onepanel(selenium[browser_id]).content.spaces.spaces[space]


@when(parsers.parse('user of {browser_id} sees that list of supported spaces '
                    'is empty in Spaces page in Onepanel'))
@then(parsers.parse('user of {browser_id} sees that list of supported spaces '
                    'is empty in Spaces page in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_assert_supported_spaces_list_is_empty(selenium, browser_id, onepanel):
    count = onepanel(selenium[browser_id]).content.spaces.spaces.count()
    assert count == 0, \
        'There is(are) {} supported spaces instead of expected none'.format(count)


@when(parsers.re(r'user of (?P<browser_id>.*?) selects (?P<strategy>.*?) '
                 r'strategy from strategy selector in (?P<conf>IMPORT|UPDATE) '
                 r'CONFIGURATION in support space form in Onepanel'))
@then(parsers.re(r'user of (?P<browser_id>.*?) selects (?P<strategy>.*?) '
                 r'strategy from strategy selector in (?P<conf>IMPORT|UPDATE) '
                 r'CONFIGURATION in support space form in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_select_strategy_in_conf_in_support_space_form(selenium, browser_id,
                                                     strategy, conf, onepanel):
    config = getattr(onepanel(selenium[browser_id]).content.spaces.
                     form, conf.lower() + '_configuration')
    strategy_selector = config.strategy_selector
    strategy_selector.expand()
    strategy_selector.options[strategy].click()


@when(parsers.re(r'user of (?P<browser_id>.*?) types "(?P<text>.*?)" '
                 r'to (?P<input_box>.*) input field in (?P<conf>IMPORT|UPDATE) '
                 r'CONFIGURATION in support space form in Onepanel'))
@then(parsers.re(r'user of (?P<browser_id>.*?) types "(?P<text>.*?)" '
                 r'to (?P<input_box>.*) input field in (?P<conf>IMPORT|UPDATE) '
                 r'CONFIGURATION in support space form in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_text_to_input_box_in_conf_in_space_support_form(selenium, browser_id,
                                                            text, input_box,
                                                            conf, onepanel):
    config = getattr(onepanel(selenium[browser_id]).content.spaces.form,
                     conf.lower() + '_configuration')
    setattr(config, transform(input_box), text)


@when(parsers.re(r'user of (?P<browser_id>.*?) enables '
                 r'(?P<toggle>Write once|Delete enabled) option '
                 r'UPDATE CONFIGURATION in support space form in Onepanel'))
@then(parsers.re(r'user of (?P<browser_id>.*?) enables '
                 r'(?P<toggle>Write once|Delete enabled) option '
                 r'UPDATE CONFIGURATION in support space form in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_enable_option_box_in_conf_in_space_support_form(selenium, browser_id,
                                                       toggle, onepanel):
    conf = onepanel(selenium[browser_id]).content.spaces.form.update_configuration
    getattr(conf, transform(toggle)).check()


@when(parsers.re(r'user of (?P<browser_id>.*?) selects (?P<strategy>.*?) '
                 r'strategy from strategy selector in (?P<conf>IMPORT|UPDATE) '
                 r'CONFIGURATION in "(?P<space>.*?)" record '
                 r'in Spaces page in Onepanel'))
@then(parsers.re(r'user of (?P<browser_id>.*?) selects (?P<strategy>.*?) '
                 r'strategy from strategy selector in (?P<conf>IMPORT|UPDATE) '
                 r'CONFIGURATION in "(?P<space>.*?)" record '
                 r'in Spaces page in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_select_strategy_in_conf_in_space_record(selenium, browser_id, strategy,
                                               conf, space, onepanel):
    config = getattr(onepanel(selenium[browser_id]).content.spaces.
                     spaces[space], conf.lower() + '_configuration')
    strategy_selector = config.strategy_selector
    strategy_selector.expand()
    strategy_selector.options[strategy].click()


@when(parsers.re(r'user of (?P<browser_id>.*?) types "(?P<text>.*?)" '
                 r'to (?P<input_box>.*) input field in (?P<conf>IMPORT|UPDATE) '
                 r'CONFIGURATION in "(?P<space>.*?)" record '
                 r'in Spaces page in Onepanel'))
@then(parsers.re(r'user of (?P<browser_id>.*?) types "(?P<text>.*?)" '
                 r'to (?P<input_box>.*) input field in (?P<conf>IMPORT|UPDATE) '
                 r'CONFIGURATION in "(?P<space>.*?)" record '
                 r'in Spaces page in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_text_to_input_box_in_conf_in_space_record(selenium, browser_id,
                                                      text, input_box, conf,
                                                      space, onepanel):
    config = getattr(onepanel(selenium[browser_id]).content.spaces.
                     spaces[space], conf.lower() + '_configuration')
    setattr(config, transform(input_box), text)


@when(parsers.re(r'user of (?P<browser_id>.*?) enables '
                 r'(?P<toggle>Write once|Delete enabled) option UPDATE '
                 r'CONFIGURATION in "(?P<space>.*?)" record in Spaces page '
                 r'in Onepanel'))
@then(parsers.re(r'user of (?P<browser_id>.*?) enables '
                 r'(?P<toggle>Write once|Delete enabled) option UPDATE '
                 r'CONFIGURATION in "(?P<space>.*?)" record in Spaces page '
                 r'in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_enable_option_box_in_conf_in_space_record(selenium, browser_id,
                                                 toggle, space, onepanel):
    config = (onepanel(selenium[browser_id]).content.spaces.
              spaces[space].update_configuration)
    getattr(config, transform(toggle)).check()


@when(parsers.parse('user of {browser_id} clicks on Save configuration '
                    'button in "{space}" record in Spaces page in Onepanel'))
@then(parsers.parse('user of {browser_id} clicks on Save configuration '
                    'button in "{space}" record in Spaces page in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_clicks_on_save_config_btn_in_space_record(selenium, browser_id,
                                                 space, onepanel):
    (onepanel(selenium[browser_id]).content.spaces.
     spaces[space].save_configuration())


@when(parsers.parse('user of {browser_id} expands "{space}" record on '
                    'spaces list in Spaces page in Onepanel'))
@then(parsers.parse('user of {browser_id} expands "{space}" record on '
                    'spaces list in Spaces page in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_expand_space_item_in_spaces_page_op_panel(selenium, browser_id,
                                                 space, onepanel):
    onepanel(selenium[browser_id]).content.spaces.spaces[space].expand()


@when(parsers.re('user of (?P<browser_id>.*?) sees that '
                 '(?P<conf>Import|Update) strategy configuration '
                 'for "(?P<space>.*?)" is as follow:\n(?P<values>.*)'))
@then(parsers.re('user of (?P<browser_id>.*?) sees that '
                 '(?P<conf>Import|Update) strategy configuration '
                 'for "(?P<space>.*?)" is as follow:\n(?P<values>.*)'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_assert_proper_space_configuration_in_panel(selenium, browser_id, conf,
                                                  space, values, onepanel):
    """Assert configuration displayed in space record in panel.

    conf should be in yaml format exactly as seen in panel, e.g.

        Update strategy: Simple scan
        Max depth: 20
        Scan interval [s]: 10
        Write once: true
        Delete enabled: false

    """
    record = onepanel(selenium[browser_id]).content.spaces.spaces[space]
    displayed_conf = getattr(record, conf.lower() + '_strategy')
    for attr, val in yaml.load(values).items():
        displayed_val = displayed_conf[attr]
        assert val.lower() == displayed_val.lower(), \
            'Displayed {} as {} instead of expected {} in {} of ' \
            '"{}" configuration'.format(displayed_val, attr, val, conf, space)


@when(parsers.parse('user of {browser_id} copies Id of "{space}" space '
                    'in Spaces page in Onepanel'))
@then(parsers.parse('user of {browser_id} copies Id of "{space}" space '
                    'in Spaces page in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_copy_space_id_in_spaces_page_in_onepanel(selenium, browser_id,
                                                space, onepanel, tmp_memory):
    space_id = onepanel(selenium[browser_id]).content.spaces.spaces[space].Id
    tmp_memory['spaces'][space] = space_id


@when(parsers.parse('user of {browser_id} expands toolbar for "{space}" '
                    'space record in Spaces page in Onepanel'))
@then(parsers.parse('user of {browser_id} expands toolbar for "{space}" '
                    'space record in Spaces page in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_expands_toolbar_icon_for_space_in_onepanel(selenium, browser_id,
                                                  space, onepanel):
    record = onepanel(selenium[browser_id]).content.spaces.spaces[space]
    record.toolbar.click()


@when(parsers.re(r"user of (?P<browser_id>.*?) clicks on "
                 r"(?P<option>Revoke space support|"
                 r"Configure data synchronization|"
                 r"Cancel sync. configuration) "
                 r"option in space's toolbar in Onepanel"))
@then(parsers.re(r"user of (?P<browser_id>.*?) clicks on "
                 r"(?P<option>Revoke space support|"
                 r"Configure data synchronization|"
                 r"Cancel sync. configuration) "
                 r"option in space's toolbar in Onepanel"))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_clicks_on_btn_in_space_toolbar_in_panel(selenium, browser_id,
                                               option, popups):
    toolbar = popups(selenium[browser_id]).toolbar
    if toolbar.is_displayed():
        toolbar.options[option].click()
    else:
        raise RuntimeError('no space toolbar found in Onepanel')


@when(parsers.re(r'user of (?P<browser_id>.*?) clicks on '
                 r'(?P<button>Yes, revoke|No, keep the support) '
                 r'button in REVOKE SPACE SUPPORT modal in Onepanel'))
@then(parsers.re(r'user of (?P<browser_id>.*?) clicks on '
                 r'(?P<button>Yes, revoke|No, keep the support) '
                 r'button in REVOKE SPACE SUPPORT modal in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_clicks_on_btn_in_revoke_space_support(selenium, browser_id,
                                             button, modals):
    modal = modals(selenium[browser_id]).revoke_space_support
    if button == 'Yes, revoke':
        modal.yes()
    else:
        modal.no()
