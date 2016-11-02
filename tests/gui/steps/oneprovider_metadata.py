# coding=utf-8
"""Steps for features of Oneprovider metadata.
"""

__author__ = "Michał Ćwiertnia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


import re
import os
import time

from tests.gui.utils.inspect import selector
from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND, MAX_REFRESH_COUNT
from tests.gui.utils.generic import click_on_element
from pytest_bdd import when, then, parsers
from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains

from tests.gui.utils.generic import refresh_and_call
from pytest_selenium_multi.pytest_selenium_multi import select_browser


import tests.gui.utils.file_system as fs
from tests.utils.acceptance_utils import list_parser


def _get_items_with_opened_metadata_panel_from_file_list(driver, name, type):
    files = driver.find_elements_by_css_selector('table.files-table tr.metadata-opened td '
                                                 '.file-icon .oneicon, '
                                                 'table.files-table tr.metadata-opened td '
                                                 '.file-label')
    icons, labels = files[::2], files[1::2]
    return [label for icon, label in zip(icons, labels)
            if label.text == name and type in icon.get_attribute('class')]


@when(parsers.parse('user of {browser_id} sees that metadata panel for {item_type} '
                    '"{item_name}" in files list has appeared'))
@then(parsers.parse('user of {browser_id} sees that metadata panel for {item_type} '
                    '"{item_name}" in files list has appeared'))
def check_if_metadata_panel_fir_file_is_displayed(selenium, browser_id, item_name, item_type):
    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until(
        lambda s: _get_items_with_opened_metadata_panel_from_file_list(driver, item_name, item_type)
    )


@when(parsers.parse('user of {browser_id} deselects {item_list} from files list'))
@then(parsers.parse('user of {browser_id} deselects {item_list} from files list'))
def deselect_items_from_file_list(selenium, browser_id, item_list):
    driver = select_browser(selenium, browser_id)

    items = {item.text: item for item in
             driver.find_elements_by_css_selector('table.files-table '
                                                  'tr[class$="active"] '
                                                  'td.file-list-col-file')}
    for item in list_parser(item_list):
        if item in items:
            item = items[item]
            Wait(driver, WAIT_FRONTEND).until(
                lambda _: item.is_displayed() and item.is_enabled(),
                message='clicking on {:s} in file list'.format(item.text)
            )
            item.click()


@then(parsers.parse('user of {browser_id} sees {tab_list} navigation tabs in opened metadata '
                    'panel'))
def check_if_navigation_tabs_for_metadata_panel_are_displayed(selenium, browser_id, tab_list):
    driver = select_browser(selenium, browser_id)
    navigation_tabs = driver.find_elements_by_css_selector('table.files-table .metadata-panel '
                                                           'ul.nav-tabs a')
    navigation_tabs = [x.text.lower() for x in navigation_tabs]
    for tab in list_parser(tab_list):
        assert tab.lower() in navigation_tabs


def _find_input_box(d, input_box_name):
    input_boxes = d.find_elements_by_css_selector('.metadata-panel table.metadata-basic-table '
                                                  'input')
    for input_box in input_boxes:
        if input_box.get_attribute('placeholder').lower() == input_box_name.lower():
            return input_box


@when(parsers.parse('user of {browser_id} clicks on "{input_box_name}" input box'))
def _click_on_input_box_in_metadata_panel(selenium, browser_id, input_box_name):

    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until(
        lambda s: _find_input_box(driver, input_box_name),
        message='clicking on {:s} input box'
    ).click()


@when(parsers.parse('user of {browser_id} clicks on "{button_name}" button in metadata panel'))
@then(parsers.parse('user of {browser_id} clicks on "{button_name}" button in metadata panel'))
def click_on_button_in_metadata_panel(selenium, browser_id, button_name):
    driver = select_browser(selenium, browser_id)
    if button_name.lower() == "save all changes":
        css_selector = '.metadata-panel .save-metadata-row button ' \
                       'span.spin-button-label'
    else:
        css_selector = '.metadata-panel .save-metadata-row button'
    click_on_element(driver, css_selector, button_name,
                     'clicking on {:s} button in metadata panel')


def _check_for_item_in_given_list(driver, name):
    def _assert_one_item_in_list(s, item_name):
        items = s.find_elements_by_css_selector('nav.secondary-sidebar ul.shares-list li .truncate')
        return sum(1 for li in items if li.text == item_name) == 1

    Wait(driver, MAX_REFRESH_COUNT * WAIT_BACKEND).until(
        lambda s: refresh_and_call(s, _assert_one_item_in_list,
                                   name),
        message='searching for exactly one {item} '
                'on shares list'.format(item=name)
    )


@when(parsers.parse('user of {browser_id} sees "{share_name}" share in shares list'))
@then(parsers.parse('user of {browser_id} sees "{share_name}" share in shares list'))
def check_if_share_is_in_shares_list(selenium, browser_id, share_name):
    driver = select_browser(selenium, browser_id)
    _check_for_item_in_given_list(driver, share_name)


@when(parsers.parse('user of {browser_id} selects "{share_name}" share from shares list'))
@then(parsers.parse('user of {browser_id} selects "{share_name}" share from shares list'))
def select_share_from_share_list(selenium, browser_id, share_name):
    driver = select_browser(selenium, browser_id)
    click_on_element(driver, 'nav.secondary-sidebar ul.shares-list li .truncate', share_name,
                     'clicking on {:s} share in shares list', ignore_case=False)


def _find_metadata_record(driver, attribute_name, unsaved_record=False):
    records = driver.find_elements_by_css_selector('.metadata-panel table.metadata-basic-table tr')
    for record in records:
        if unsaved_record:
            if record.find_element_by_css_selector('th input').get_attribute('value') == attribute_name:
                return record
        else:
            if record.find_element_by_css_selector('th').text == attribute_name:
                return record
    return False


@then(parsers.parse('user of {browser_id} should not see new metadata record with attribute '
                    '"{attribute_name}"'))
def check_if_new_metadata_record_has_not_appeared(selenium, browser_id, attribute_name):
    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until_not(
        lambda s: _find_metadata_record(driver, attribute_name),
        message='checking there is no metadata with'
                'attribute {attribute}'.format(attribute=attribute_name)
    )


@when(parsers.parse('user of {browser_id} sees metadata record with attribute "{attribute_name}" '
                    'and value "{value_name}" in metadata-panel in shares view'))
@then(parsers.parse('user of {browser_id} sees metadata record with attribute "{attribute_name}" '
                    'and value "{value_name}" in metadata-panel in shares view'))
def check_if_metadata_record_is_displayed_in_metadata_panel_in_shares_view(selenium, browser_id,
                                                                           attribute_name,
                                                                           value_name):
    driver = select_browser(selenium, browser_id)
    record = _find_metadata_record(driver, attribute_name)
    Wait(driver, WAIT_FRONTEND).until(
        lambda s: _find_metadata_record(driver, attribute_name),
        message='searching for metadata record with attribute '
                '{attribute}'.format(attribute=attribute_name)
    )
    assert record.find_element_by_css_selector('input').get_attribute('value') == value_name


@when(parsers.parse('user of {browser_id} should see new metadata record with attribute '
                    '"{attribute_name}" and value "{value_name}"'))
@then(parsers.parse('user of {browser_id} should see new metadata record with attribute '
                    '"{attribute_name}" and value "{value_name}"'))
def check_if_new_metadata_record_has_appeared(selenium, browser_id, attribute_name, value_name):
    driver = select_browser(selenium, browser_id)
    record = _find_metadata_record(driver, attribute_name)
    Wait(driver, WAIT_FRONTEND).until(
        lambda s: _find_metadata_record(driver, attribute_name),
        message='searching for metadata record with attribute '
                '{attribute}'.format(attribute=attribute_name)
    )
    assert record.find_element_by_css_selector('td:not([class])').text == value_name


@then(parsers.parse('user of {browser_id} should not see any metadata record for "{file_name}"'))
def check_if_all_metadata_records_have_been_deleted(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    records = driver.find_elements_by_css_selector('table.files-table .metadata-panel '
                                                   'table.metadata-basic-table tr')
    assert len(records) == 1


@when(parsers.parse('user of {browser_id} clicks on delete metadata record icon for '
                    'metadata record with attribute "{attribute_name}"'))
def click_delete_metadata_record_button(selenium, browser_id, attribute_name):
    driver = select_browser(selenium, browser_id)
    files_table = driver.find_element_by_css_selector('table.files-table')
    record = _find_metadata_record(files_table, attribute_name)
    Wait(driver, WAIT_FRONTEND).until(
        lambda s: _find_metadata_record(driver, attribute_name),
        message='searching for metadata record with attribute '
                '{attribute}'.format(attribute=attribute_name)
    )
    click_on_element(record, 'td span.oneicon-close', '',
                     'clicking on delete icon for metadata record with attribute '
                     '{attribute}'.format(attribute=attribute_name))


@when(parsers.parse('user of {browser_id} clicks on add icon in metadata panel'))
def click_on_add_button_in_metadata_panel(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    click_on_element(driver, '.metadata-panel table.metadata-basic-table '
                             'span.oneicon-add', '', 'clicking on add metadata record icon')


@when(parsers.parse('user of {browser_id} clicks on "{input_box_name}" input box for metadata '
                    'record with attribute "{attribute_name}"'))
def click_input_box_for_metadata_record(selenium, browser_id, input_box_name, attribute_name):
    driver = select_browser(selenium, browser_id)
    driver = Wait(driver, WAIT_FRONTEND).until(
        lambda s: _find_metadata_record(driver, attribute_name),
        message='searching for metadata record with attribute '
                '{attribute}'.format(attribute=attribute_name)
    )
    click_on_element(driver, 'input', '', 'clicking on {input_box_name} input '
                                          'box'.format(input_box_name=input_box_name))


@when(parsers.parse('user of {browser_id} clicks on "{input_box_name}" input box for new metadata '
                    'record with attribute "{attribute_name}"'))
def click_input_box_for_new_metadata_record(selenium, browser_id, input_box_name, attribute_name):
    driver = select_browser(selenium, browser_id)
    driver = Wait(driver, WAIT_FRONTEND).until(
        lambda s: _find_metadata_record(driver, attribute_name, unsaved_record=True),
        message='searching for new metadata record'
    )
    click_on_element(driver, 'input[placeholder="Value"]', '', 'clicking on {input_box_name} input '
                                          'box'.format(input_box_name=input_box_name))
#
#
# @then(parsers.parse('user of {browser_id} sees that "{button_name}" button is disabled'))
# def check_if_button_is_disabled(selenium, browser_id, button_name):
#     driver = select_browser(selenium, browser_id)
#     button = driver.find_element_by_css_selector('.metadata-panel .save-metadata-row button '
#                                                  'span.spin-button-label')
#     Wait(driver, WAIT_FRONTEND).until_not(
#         lambda s: button.is_enabled()
#     )
#     # Wait(driver, WAIT_FRONTEND).until_not(
#     #     EC.element_to_be_clickable((By.CSS_SELECTOR, '.metadata-panel .save-metadata-row button '
#     #                                                  'span.spin-button-label')),
#     #     message='checking if {button_name} is not clickable'.format(button_name=button_name)
#     # )


def _get_items_from_file_list_with_metadata_icon(driver, name, type, visibility=''):
    files = driver.find_elements_by_css_selector('table.files-table td.file-list-col-file')
    for file in files:
        icon = file.find_element_by_css_selector('.file-icon span.oneicon')
        label = file.find_element_by_css_selector('.file-label .truncate').text
        if type in icon.get_attribute('class') and label == name:
            metadata_tool = file.find_element_by_css_selector('.file-tool-metadata')
            if 'visible-on-parent-hover' + visibility in metadata_tool.get_attribute('class'):
                return file
    return None


@when(parsers.parse('user of {browser_id} should not see metadata icon for {item_type} '
                    '"{item_name}"'))
def check_if_metadata_icon_for_item_is_hidden(selenium, browser_id, item_name, item_type):
    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until(
        lambda s: _get_items_from_file_list_with_metadata_icon(driver, item_name, item_type),
        message='checking if metadata icon for {item_type} {item_name} is hidden'.format(
            item_type=item_type, item_name=item_name)
    )


@then(parsers.parse('user of {browser_id} sees metadata icon for {item_type} "{item_name}"'))
def check_if_metadata_icon_is_displayed(selenium, browser_id, item_type, item_name):
    driver = select_browser(selenium, browser_id)

    Wait(driver, WAIT_FRONTEND).until(
        lambda s: _get_items_from_file_list_with_metadata_icon(driver, item_name, item_type,
                                                               visibility='-25p'),
        message='checking if metadata icon for {item_type} {item_name} is visible'.format(
            item_type=item_type, item_name=item_name)
    )


@then(parsers.parse('user of {browser_id} sees that entered metadata record with attribute '
                    '"{attribute_name}" is red'))
def check_if_entered_metadata_record_is_red(selenium, browser_id, attribute_name):
    driver = select_browser(selenium, browser_id)
    record = _find_metadata_record(driver, attribute_name, unsaved_record=True)
    assert 'invalid' in record.get_attribute('class')


@when(parsers.parse('user of {browser_id} clicks on "{tab_name}" navigation tab in metadata '
                    'panel'))
@then(parsers.parse('user of {browser_id} clicks on "{tab_name}" navigation tab in metadata '
                    'panel'))
def click_on_navigation_tab_in_metadata_panel(selenium, browser_id, tab_name):
    driver = select_browser(selenium, browser_id)
    click_on_element(driver, 'table.files-table .metadata-panel ul.nav-tabs li', tab_name,
                     'clicking on {:s} navigation tab in metadata panel')


@when(parsers.parse('user of {browser_id} clicks on textarea in "{tab_name}" navigation tab'))
def click_on_textarea_in_navigation_tab(selenium, browser_id, tab_name):
    driver = select_browser(selenium, browser_id)
    click_on_element(driver, 'table.files-table .metadata-panel '
                             '.metadata-{tab_name}-editor textarea'.format(tab_name=tab_name.lower()),
                     '', 'clicking on textarea for {tab_name} metadata'.format(tab_name=tab_name))


@when(parsers.parse('user of {browser_id} sees that textarea in "{tab_name}" navigation tab '
                    'has got "{metadata_record}" metadata record'))
@then(parsers.parse('user of {browser_id} sees that textarea in "{tab_name}" navigation tab '
                    'has got "{metadata_record}" metadata record'))
def check_if_textarea_in_navigation_tab_has_got_metadata_record(selenium, browser_id,
                                                                metadata_record, tab_name):
    driver = select_browser(selenium, browser_id)
    textarea = driver.find_element_by_css_selector('table.files-table .metadata-panel '
                                                   '.metadata-{tab_name}-editor '
                                                   'textarea'.format(tab_name=tab_name.lower()))
    assert metadata_record in textarea.get_attribute('value')


@when(parsers.parse('user of {browser_id} clears textarea in "{tab_name}" navigation tab'))
def clear_textarea_in_navigation_tab(selenium, browser_id, tab_name):
    driver = select_browser(selenium, browser_id)
    driver.find_element_by_css_selector('table.files-table .metadata-panel '
                                        '.metadata-{tab_name}-editor '
                                        'textarea'.format(tab_name=tab_name.lower())).clear()


@then(parsers.parse('user of {browser_id} should see that textarea in "{tab_name}" navigation tab '
                    'hasn\'t got any metadata record'))
def check_if_textarea_in_navigation_tabhasnt_got_any_metadata_record(selenium, browser_id,
                                                                     tab_name):
    driver = select_browser(selenium, browser_id)
    textarea = driver.find_element_by_css_selector('table.files-table .metadata-panel '
                                                   '.metadata-{tab_name}-editor '
                                                   'textarea'.format(tab_name=tab_name.lower()))
    if tab_name.lower() == 'json':
        assert textarea.get_attribute('value') == '{}'
    else:
        assert textarea.get_attribute('value') == ''


@when(parsers.parse('user of {browser_id} sees that metadata panel for {item_type} "{item_name}" '
                    'has disappeared'))
@then(parsers.parse('user of {browser_id} sees that metadata panel for {item_type} "{item_name}" '
                    'has disappeared'))
def check_if_metadata_panel_has_disappeared(selenium, browser_id, item_type, item_name):
    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until_not(
        lambda s: _get_items_with_opened_metadata_panel_from_file_list(driver, item_name,
                                                                         item_type)
    )


def tmp_name(driver, name, type):
    files = driver.find_elements_by_css_selector('table.files-table td.file-list-col-file')
    for file in files:
        icon = file.find_element_by_css_selector('.file-icon span.oneicon')
        label = file.find_element_by_css_selector('.file-label .truncate').text
        if type in icon.get_attribute('class') and label == name:
            return file
    return None


@when(parsers.parse('user of {browser_id} clicks the metadata icon for {item_type} "{item_name}" '
                    'in files list'))
def click_metadata_icon_for_item(selenium, browser_id, item_type, item_name):
    driver = select_browser(selenium, browser_id)
    file = Wait(driver, WAIT_FRONTEND).until(
        lambda s: tmp_name(driver, item_name, item_type),
        message='checking if metadata icon for {item_type} {item_name} is visible'.format(
            item_type=item_type, item_name=item_name)
    )
    click_on_element(file, '.file-tool-metadata', '', 'clicking on metadata icon for {type} '
                                                      '{item}'.format(type=item_type,
                                                                      item=item_name))






