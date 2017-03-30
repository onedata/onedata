# coding=utf-8
"""Steps for features of Oneprovider metadata.
"""

from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.generic import parse_seq, repeat_failed

from pytest_bdd import when, then, parsers


__author__ = "Michał Ćwiertnia, Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@when(parsers.parse('user of {browser_id} sees that metadata panel for '
                    '"{item_name}" in files list is displayed'))
@then(parsers.parse('user of {browser_id} sees that metadata panel for '
                    '"{item_name}" in files list is displayed'))
@when(parsers.parse('user of {browser_id} sees that metadata panel for '
                    '"{item_name}" in files list has appeared'))
@then(parsers.parse('user of {browser_id} sees that metadata panel for '
                    '"{item_name}" in files list has appeared'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_files_metadata_panel_displayed(browser_id, item_name, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    browser.get_metadata_for(item_name)


@when(parsers.parse('user of {browser_id} sees that metadata panel for '
                    '"{item_name}" in files list is not displayed'))
@then(parsers.parse('user of {browser_id} sees that metadata panel for '
                    '"{item_name}" in files list is not displayed'))
@when(parsers.parse('user of {browser_id} sees that metadata panel for '
                    '"{item_name}" in files list has disappeared'))
@then(parsers.parse('user of {browser_id} sees that metadata panel for '
                    '"{item_name}" in files list has disappeared'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_not_files_metadata_panel_displayed(browser_id, item_name, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    try:
        browser.get_metadata_for(item_name)
    except RuntimeError:
        pass
    else:
        raise RuntimeError('metadata for "{}" found in file browser '
                           'while it should not be'.format(item_name))


@when(parsers.parse('user of {browser_id} sees {tab_list} navigation tabs in '
                    'metadata panel opened for "{item_name}"'))
@then(parsers.parse('user of {browser_id} sees {tab_list} navigation tabs in '
                    'metadata panel opened for "{item_name}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def are_nav_tabs_for_metadata_panel_displayed(browser_id, tab_list, item_name,
                                              tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    nav = browser.get_metadata_for(item_name).navigation
    for tab in parse_seq(tab_list):
        assert getattr(nav, tab.lower()) is not None, \
            'no navigation tab {} found'.format(tab)


@when(parsers.parse('user of {browser_id} types "{text}" to attribute input '
                    'box of new metadata basic entry in metadata panel '
                    'opened for "{item_name}"'))
@then(parsers.parse('user of {browser_id} types "{text}" to attribute input '
                    'box of new metadata basic entry in metadata panel '
                    'opened for "{item_name}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def type_text_to_attr_input_in_new_basic_entry(browser_id, text, item_name,
                                               tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    browser.get_metadata_for(item_name).basic.new_entry.attribute = text


@when(parsers.parse('user of {browser_id} types "{text}" to value input '
                    'box of new metadata basic entry in metadata panel '
                    'opened for "{item_name}"'))
@then(parsers.parse('user of {browser_id} types "{text}" to value input '
                    'box of new metadata basic entry in metadata panel '
                    'opened for "{item_name}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def type_text_to_val_input_in_new_basic_entry(browser_id, text, item_name,
                                              tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    browser.get_metadata_for(item_name).basic.new_entry.value = text


@when(parsers.parse('user of {browser_id} clicks on "{button_name}" button '
                    'in metadata panel opened for "{item_name}"'))
@then(parsers.parse('user of {browser_id} clicks on "{button_name}" button '
                    'in metadata panel opened for "{item_name}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_on_button_in_metadata_panel(browser_id, button_name,
                                      item_name, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    metadata_row = browser.get_metadata_for(item_name)
    btn = getattr(metadata_row, button_name.lower().replace(' ', '_'))()
    btn()


@when(parsers.parse('user of {browser_id} sees that "{button_name}" button '
                    'in metadata panel opened for "{item_name}" is disabled'))
@then(parsers.parse('user of {browser_id} sees that "{button_name}" button '
                    'in metadata panel opened for "{item_name}" is disabled'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_btn_disabled_in_metadata_footer(browser_id, button_name,
                                           item_name, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    metadata_row = browser.get_metadata_for(item_name)
    btn = getattr(metadata_row, button_name.lower().replace(' ', '_'))()
    assert not btn.is_enabled()


@when(parsers.parse('user of {browser_id} should not see basic metadata entry '
                    'with attribute named "{attribute_name}" in metadata panel '
                    'opened for "{item_name}"'))
@then(parsers.parse('user of {browser_id} should not see basic metadata entry '
                    'with attribute named "{attribute_name}" in metadata panel '
                    'opened for "{item_name}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_there_is_no_such_meta_record(browser_id, attribute_name,
                                        item_name, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    metadata_row = browser.get_metadata_for(item_name)
    assert attribute_name not in metadata_row.basic.entries, \
        'metadata entry "{}" found, while should not be'.format(attribute_name)


@when(parsers.parse('user of {browser_id} should see basic metadata entry '
                    'with attribute named "{attr_name}" and value "{attr_val}" '
                    'in metadata panel opened for "{item_name}"'))
@then(parsers.parse('user of {browser_id} should see basic metadata entry '
                    'with attribute named "{attr_name}" and value "{attr_val}" '
                    'in metadata panel opened for "{item_name}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_there_is_such_meta_record(browser_id, attr_name, attr_val,
                                     item_name, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    metadata_row = browser.get_metadata_for(item_name)
    err_msg = 'no metadata entry "{}" with value "{}" found'.format(attr_name,
                                                                    attr_val)
    assert metadata_row.basic.entries[attr_name].value == attr_val, err_msg


@when(parsers.parse('user of {browser_id} should not see any basic metadata '
                    'entry in metadata panel opened for "{item_name}"'))
@then(parsers.parse('user of {browser_id} should not see any basic metadata '
                    'entry in metadata panel opened for "{item_name}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_lack_of_metadata_entries(browser_id, item_name, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    metadata_row = browser.get_metadata_for(item_name)
    entries = metadata_row.basic.entries
    assert entries.count() == 0, \
        'found metadata entries for "{}" while not expected'.format(item_name)


@when(parsers.parse('user of {browser_id} clicks on delete basic metadata entry '
                    'icon for basic metadata entry with attribute named '
                    '"{attr_name}" in metadata panel opened for "{item_name}"'))
@then(parsers.parse('user of {browser_id} clicks on delete basic metadata entry '
                    'icon for basic metadata entry with attribute named '
                    '"{attr_name}" in metadata panel opened for "{item_name}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_on_del_metadata_record_button(browser_id, attr_name, item_name,
                                        tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    browser.get_metadata_for(item_name).basic.entries[attr_name].remove()


@when(parsers.parse('user of {browser_id} clicks on add basic metadata '
                    'entry icon in metadata panel opened for "{item_name}"'))
@then(parsers.parse('user of {browser_id} clicks on add basic metadata '
                    'entry icon in metadata panel opened for "{item_name}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_on_add_meta_rec_btn_in_metadata_panel(browser_id, item_name,
                                                tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    browser.get_metadata_for(item_name).basic.new_entry.add()


@when(parsers.parse('user of {browser_id} sees that edited attribute key in '
                    'metadata panel opened for "{item_name}" is highlighted '
                    'as invalid'))
@then(parsers.parse('user of {browser_id} sees that edited attribute key in '
                    'metadata panel opened for "{item_name}" is highlighted '
                    'as invalid'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_entered_attr_key_is_invalid(browser_id, item_name, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    entry = browser.get_metadata_for(item_name).basic.new_entry
    assert not entry.is_valid(), \
        'basic metadata new entry for "{}" is valid, ' \
        'while it should not'.format(item_name)


@when(parsers.parse('user of {browser_id} clicks on {tab_name} navigation '
                    'tab in metadata panel opened for "{item_name}"'))
@then(parsers.parse('user of {browser_id} clicks on {tab_name} navigation '
                    'tab in metadata panel opened for "{item_name}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_on_navigation_tab_in_metadata_panel(browser_id, tab_name,
                                              item_name, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    tab = getattr(browser.get_metadata_for(item_name).navigation,
                  tab_name.lower())
    tab()


@when(parsers.re('user of (?P<browser_id>.+?) sees that (?P<tab>JSON|RDF) '
                 'textarea placed in metadata panel opened for '
                 '"(?P<item_name>.+?)" contains "(?P<metadata_record>.+?)"'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that (?P<tab>JSON|RDF) '
                 'textarea placed in metadata panel opened for '
                 '"(?P<item_name>.+?)" contains "(?P<metadata_record>.+?)"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_textarea_contains_record(browser_id, metadata_record, tab,
                                    item_name, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    metadata_row = browser.get_metadata_for(item_name)
    tab = getattr(metadata_row, tab.lower())
    assert metadata_record in tab.text_area, \
        'text in textarea : {} does not contain {}'.format(tab.textarea,
                                                           metadata_record)


@when(parsers.re('user of (?P<browser_id>.+?) sees that content of '
                 '(?P<tab>JSON|RDF) textarea placed in metadata panel opened '
                 'for "(?P<item_name>.+?)" is equal to: "(?P<content>.*?)"'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that content of '
                 '(?P<tab>JSON|RDF) textarea placed in metadata panel opened '
                 'for "(?P<item_name>.+?)" is equal to: "(?P<content>.*?)"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_textarea_content_is_eq_to(browser_id, item_name, content,
                                     tab, tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    metadata_row = browser.get_metadata_for(item_name)
    tab = getattr(metadata_row, tab.lower())
    assert tab.text_area == content, \
        'expected: {}, got: {} in textarea for ' \
        'metadata in {}'.format(content, tab.textarea, tab)


@when(parsers.re('user of (?P<browser_id>.+?) types "(?P<text>.+?)" '
                 'to (?P<tab>JSON|RDF) textarea placed in metadata panel '
                 'opened for "(?P<item_name>.+?)"'))
@then(parsers.re('user of (?P<browser_id>.+?) types "(?P<text>.+?)" '
                 'to (?P<tab>JSON|RDF) textarea placed in metadata panel '
                 'opened for "(?P<item_name>.+?)"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def type_text_to_metadata_textarea(browser_id, item_name, text, tab,
                                   tmp_memory):
    browser = tmp_memory[browser_id]['file_browser']
    metadata_row = browser.get_metadata_for(item_name)
    tab = getattr(metadata_row, tab.lower())
    tab.text_area = text
