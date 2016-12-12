# coding=utf-8
"""Steps for features of Oneprovider metadata.
"""

__author__ = "Michał Ćwiertnia, Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.generic import parse_seq
from tests.gui.steps.oneprovider_file_list import _get_items_from_file_list, type_to_icon

from pytest_bdd import when, then, parsers
from selenium.webdriver.support.ui import WebDriverWait as Wait

from pytest_selenium_multi.pytest_selenium_multi import select_browser


def _get_metadata_panel_for_file(files, file_name, file_type, exception=True):
    file_row = files.get(file_name, None)
    icon = type_to_icon[file_type]
    if file_row and icon in file_row[2].get_attribute('class'):
        return file_row[6]
    elif exception:
        raise RuntimeError('no metadata panel for {} named "{}" '
                           'found'.format(file_type, file_name))
    else:
        return None


def _get_metadata_record(panel, attr_name):
    records = panel.find_elements_by_css_selector('table.metadata-basic-table tr')
    for rec in records:
        if rec.find_element_by_css_selector('th').text == attr_name:
            return rec


def _find_input_box_using_placeholder(panel, placeholder):
    return panel.find_element_by_css_selector('input[placeholder={:s}]'
                                              ''.format(placeholder))


@when(parsers.parse('user of {browser_id} sees that metadata panel for '
                    '{item_type} named "{item_name}" in files list '
                    'is displayed'))
@then(parsers.parse('user of {browser_id} sees that metadata panel for '
                    '{item_type} named "{item_name}" in files list '
                    'is displayed'))
@when(parsers.parse('user of {browser_id} sees that metadata panel for '
                    '{item_type} named "{item_name}" in files list '
                    'has appeared'))
@then(parsers.parse('user of {browser_id} sees that metadata panel for '
                    '{item_type} named "{item_name}" in files list '
                    'has appeared'))
def is_files_metadata_panel_displayed(selenium, browser_id, item_name, item_type):
    driver = select_browser(selenium, browser_id)
    metadata_panel = Wait(driver, WAIT_FRONTEND).until(
        lambda d: _get_metadata_panel_for_file(_get_items_from_file_list(d),
                                               item_name, item_type,
                                               exception=False),
        message="waiting for '{:s}' {:s}'s metadata panel "
                "to appear".format(item_name, item_type)
    )
    assert metadata_panel.is_displayed(), \
        'metadata panel for {} appeared in dom but ' \
        'is not displayed'.format(item_name)


@when(parsers.parse('user of {browser_id} sees that metadata panel for '
                    '{item_type} named "{item_name}" in files list '
                    'is not displayed'))
@then(parsers.parse('user of {browser_id} sees that metadata panel for '
                    '{item_type} named "{item_name}" in files list '
                    'is not displayed'))
@when(parsers.parse('user of {browser_id} sees that metadata panel for '
                    '{item_type} named "{item_name}" in files list '
                    'has disappeared'))
@then(parsers.parse('user of {browser_id} sees that metadata panel for '
                    '{item_type} named "{item_name}" in files list '
                    'has disappeared'))
def is_not_files_metadata_panel_displayed(selenium, browser_id,
                                          item_type, item_name):
    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until_not(
        lambda d: _get_metadata_panel_for_file(_get_items_from_file_list(d),
                                               item_name, item_type,
                                               exception=False),
        message="waiting for '{:s}' {:s}'s metadata panel "
                "to disappear".format(item_name, item_type)
    )


@when(parsers.parse('user of {browser_id} sees {tab_list} navigation tabs in '
                    'metadata panel opened for {item_type} named "{item_name}"'))
@then(parsers.parse('user of {browser_id} sees {tab_list} navigation tabs in '
                    'metadata panel opened for {item_type} named "{item_name}"'))
def are_nav_tabs_for_metadata_panel_displayed(selenium, browser_id, tab_list,
                                              item_type, item_name):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    nav_tabs = {x.text.lower() for x
                in panel.find_elements_by_css_selector('ul.nav-tabs a')}
    for tab in parse_seq(tab_list):
        assert tab.lower() in nav_tabs


@when(parsers.parse('user of {browser_id} clicks on input box with placeholder '
                    'equal to "{placeholder}" in metadata panel opened for '
                    '{item_type} named "{item_name}"'))
@then(parsers.parse('user of {browser_id} clicks on input box with placeholder '
                    'equal to "{placeholder}" in metadata panel opened for '
                    '{item_type} named "{item_name}"'))
def click_on_input_box_with_placeholder_in_metadata_panel(selenium, browser_id,
                                                          placeholder, item_type,
                                                          item_name):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    Wait(driver, WAIT_FRONTEND).until(
        lambda _: _find_input_box_using_placeholder(panel, placeholder),
        message='clicking on input box with placeholder={:s} in metadata panel '
                'opened for {} named "{}"'.format(placeholder,
                                                  item_type,
                                                  item_name)
    ).click()


@when(parsers.parse('user of {browser_id} clicks on "{button_name}" button '
                    'in metadata panel opened for '
                    '{item_type} named "{item_name}"'))
@then(parsers.parse('user of {browser_id} clicks on "{button_name}" button '
                    'in metadata panel opened for '
                    '{item_type} named "{item_name}"'))
def click_on_button_in_metadata_panel(selenium, browser_id, button_name,
                                      item_type, item_name):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    buttons = panel.find_elements_by_css_selector('button')
    button_name = button_name.lower()
    for btn in buttons:
        if btn.text.lower() == button_name:
            btn.click()
            break
    else:
        raise RuntimeError('no button named {} found in metadata panel '
                           'for {} named "{}"'.format(button_name,
                                                      item_type,
                                                      item_name))


@when(parsers.parse('user of {browser_id} should not see basic metadata entry with '
                    'attribute named "{attribute_name}" in metadata panel '
                    'opened for {item_type} named "{item_name}"'))
@then(parsers.parse('user of {browser_id} should not see basic metadata entry with '
                    'attribute named "{attribute_name}" in metadata panel '
                    'opened for {item_type} named "{item_name}"'))
def assert_there_is_no_such_meta_record(selenium, browser_id, attribute_name,
                                        item_type, item_name):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    Wait(driver, WAIT_FRONTEND).until_not(
        lambda _: _get_metadata_record(panel, attribute_name),
        message='checking there is no metadata with'
                'attribute {attribute}'.format(attribute=attribute_name)
    )


@when(parsers.parse('user of {browser_id} should see basic metadata entry '
                    'with attribute named "{attr_name}" and value "{attr_val}" '
                    'in metadata panel opened for {item_type} named "{item_name}"'))
@then(parsers.parse('user of {browser_id} should see basic metadata entry '
                    'with attribute named "{attr_name}" and value "{attr_val}" '
                    'in metadata panel opened for {item_type} named "{item_name}"'))
def assert_there_is_such_meta_record(selenium, browser_id, attr_name,
                                     attr_val, item_type, item_name):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    attr = Wait(driver, WAIT_FRONTEND).until(
        lambda _: _get_metadata_record(panel, attr_name),
        message='searching for basic metadata entry with attribute '
                '{attribute} in metadata panel opened for {type} '
                'named {name}'.format(attribute=attr_name,
                                      type=item_type,
                                      name=item_name)
    )
    value = attr.find_element_by_css_selector('td:not([class])').text
    assert value == attr_val, 'value found for metadata attribute {} is {} ' \
                              'instead of expected {}'.format(attr_name,
                                                              value,
                                                              attr_val)


@when(parsers.parse('user of {browser_id} should not see any basic metadata entry '
                    'in metadata panel opened for {item_type} named "{item_name}"'))
@then(parsers.parse('user of {browser_id} should not see any basic metadata entry '
                    'in metadata panel opened for {item_type} named "{item_name}"'))
def is_metadata_rocords_empty(selenium, browser_id, item_type, item_name):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    records = panel.find_elements_by_css_selector('table.metadata-basic-table tr')
    assert len(records) == 1, 'there still are some basic metadata entrys for {} ' \
                              'named {} when there should ' \
                              'not be any'.format(item_type,
                                                  item_name)


@when(parsers.parse('user of {browser_id} clicks on delete basic metadata entry icon for '
                    'basic metadata entry with attribute named "{attr_name}" '
                    'in metadata panel opened for {item_type} named "{item_name}"'))
@then(parsers.parse('user of {browser_id} clicks on delete basic metadata entry icon for '
                    'basic metadata entry with attribute named "{attr_name}" '
                    'in metadata panel opened for {item_type} named "{item_name}"'))
def click_on_del_metadata_record_button(selenium, browser_id, attr_name,
                                        item_type, item_name):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    attr = Wait(driver, WAIT_FRONTEND).until(
        lambda _: _get_metadata_record(panel, attr_name),
        message='searching for basic metadata entry with attribute '
                '{attribute} in metadata panel opened for {type} '
                'named {name}'.format(attribute=attr_name,
                                      type=item_type,
                                      name=item_name)
    )
    attr.find_element_by_css_selector('.oneicon-close').click()


@when(parsers.parse('user of {browser_id} clicks on add basic metadata entry icon '
                    'in metadata panel opened for {item_type} named "{item_name}"'))
@then(parsers.parse('user of {browser_id} clicks on add basic metadata entry icon '
                    'in metadata panel opened for {item_type} named "{item_name}"'))
def click_on_add_meta_rec_btn_in_metadata_panel(selenium, browser_id,
                                                item_type, item_name):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    panel.find_element_by_css_selector('table.metadata-basic-table '
                                       'tr.basic-new-entry '
                                       '.oneicon-add').click()


@when(parsers.parse('user of {browser_id} sees that edited attribute key in '
                    'metadata panel opened for {item_type} named "{item_name}" '
                    'is highlighted as invalid'))
@then(parsers.parse('user of {browser_id} sees that edited attribute key in '
                    'metadata panel opened for {item_type} named "{item_name}" '
                    'is highlighted as invalid'))
def assert_entered_attr_key_is_invalid(selenium, browser_id,
                                       item_type, item_name):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    edit_rec = panel.find_element_by_css_selector('table.metadata-basic-table '
                                                  'tr.basic-new-entry')
    assert 'invalid' in edit_rec.get_attribute('class'), \
        'edited basic metadata entry is valid while it should not be'


@when(parsers.parse('user of {browser_id} clicks on "{tab_name}" navigation tab '
                    'in metadata panel opened for {item_type} named "{item_name}"'))
@then(parsers.parse('user of {browser_id} clicks on "{tab_name}" navigation tab '
                    'in metadata panel opened for {item_type} named "{item_name}"'))
def click_on_navigation_tab_in_metadata_panel(selenium, browser_id, tab_name,
                                              item_type, item_name):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    nav_tabs = panel.find_elements_by_css_selector('ul.nav-tabs a')
    tab_name = tab_name.lower()
    for tab in nav_tabs:
        if tab.text.lower() == tab_name:
            tab.click()
            break
    else:
        raise RuntimeError('no tab named {} found in metadata panel opened '
                           'for {} named "{}"'.format(tab_name,
                                                      item_type,
                                                      item_name))


@when(parsers.parse('user of {browser_id} clicks on textarea placed '
                    'in metadata panel opened for {item_type} '
                    'named "{item_name}"'))
@then(parsers.parse('user of {browser_id} clicks on textarea placed '
                    'in metadata panel opened for {item_type} '
                    'named "{item_name}"'))
def click_on_textarea_in_metadata_panel(selenium, browser_id,
                                        item_type, item_name):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    panel.find_element_by_css_selector('.tab-pane.active textarea').click()


@when(parsers.parse('user of {browser_id} clears textarea placed '
                    'in metadata panel opened for {item_type} '
                    'named "{item_name}"'))
@then(parsers.parse('user of {browser_id} clears textarea placed '
                    'in metadata panel opened for {item_type} '
                    'named "{item_name}"'))
def clear_textarea_in_metadata_panel(selenium, browser_id,
                                     item_type, item_name):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    panel.find_element_by_css_selector('.tab-pane.active textarea').clear()


@when(parsers.parse('user of {browser_id} sees that textarea placed '
                    'in metadata panel opened for {item_type} named '
                    '"{item_name}" contains "{metadata_record}"'))
@then(parsers.parse('user of {browser_id} sees that textarea placed '
                    'in metadata panel opened for {item_type} named '
                    '"{item_name}" contains "{metadata_record}"'))
def assert_textarea_contains_record(selenium, browser_id, metadata_record,
                                    item_type, item_name):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    textarea = panel.find_element_by_css_selector('.tab-pane.active textarea')
    val = textarea.get_attribute('value')
    assert metadata_record in val, \
        'text in textarea : {} does not contain {}'.format(val, metadata_record)


@when(parsers.re('user of (?P<browser_id>.+?) sees that content of textarea '
                 'placed in metadata panel opened for (?P<item_type>.+?) named '
                 '"(?P<item_name>.+?)" is equal to: "(?P<content>.*?)"'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that content of textarea '
                 'placed in metadata panel opened for (?P<item_type>.+?) named '
                 '"(?P<item_name>.+?)" is equal to: "(?P<content>.*?)"'))
def assert_textarea_content_is_eq_to(selenium, browser_id,
                                     item_type, item_name, content):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    textarea = panel.find_element_by_css_selector('.tab-pane.active textarea')
    val = textarea.get_attribute('value')
    assert val == content, 'content of textarea should be {} but is {} ' \
                           'instead'.format(content, val)
