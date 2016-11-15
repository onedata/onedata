# coding=utf-8
"""Steps for features of Oneprovider metadata.
"""

__author__ = "Michał Ćwiertnia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND, MAX_REFRESH_COUNT
from tests.gui.utils.generic import click_on_element
from tests.gui.utils.generic import parse_seq
from tests.gui.steps.oneprovider_file_list import _get_items_from_file_list, type_to_icon

from pytest_bdd import when, then, parsers
from selenium.webdriver.support.ui import WebDriverWait as Wait

from pytest_selenium_multi.pytest_selenium_multi import select_browser


def _get_metadata_panel_for_file(files, file_name, file_type):
    file_row = files.get(file_name, None)
    icon = type_to_icon[file_type]
    if file_row and icon in file_row[2].get_attribute('class'):
        return file_row[6]


def _get_metadata_record(panel, attr_name, unsaved=False):
    records = panel.find_elements_by_css_selector('table.metadata-basic-table tr')
    for rec in records:
        if unsaved:
            if attr_name == rec.find_element_by_css_selector('th input'
                                                             '.basic-new-entry-key'
                                                             ).get_attribute('value'):
                return rec
        else:
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
        lambda _: _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                               item_name, item_type),
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
        lambda _: _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                               item_name, item_type),
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
                    'equal to {placeholder} in metadata panel opened for '
                    '{item_type} named "{item_name}"'))
@then(parsers.parse('user of {browser_id} clicks on input box with placeholder '
                    'equal to {placeholder} in metadata panel opened for '
                    '{item_type} named "{item_name}"'))
def click_on_input_box_with_placeholder_in_metadata_panel(selenium, browser_id,
                                                          placeholder, item_type,
                                                          item_name):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    Wait(driver, WAIT_FRONTEND).until(
        lambda s: _find_input_box_using_placeholder(panel, placeholder),
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
        raise ValueError('no button named {} found in metadata panel '
                         'for {} named "{}"'.format(button_name,
                                                    item_type,
                                                    item_name))


# def _check_for_item_in_given_list(driver, name):
#     def _assert_one_item_in_list(s, item_name):
#         items = s.find_elements_by_css_selector('nav.secondary-sidebar ul.shares-list li .truncate')
#         return sum(1 for li in items if li.text == item_name) == 1
#
#     Wait(driver, MAX_REFRESH_COUNT * WAIT_BACKEND).until(
#         lambda s: refresh_and_call(s, _assert_one_item_in_list,
#                                    name),
#         message='searching for exactly one {item} '
#                 'on shares list'.format(item=name)
#     )
#
#
# @when(parsers.parse('user of {browser_id} sees "{share_name}" share in shares list'))
# @then(parsers.parse('user of {browser_id} sees "{share_name}" share in shares list'))
# def check_if_share_is_in_shares_list(selenium, browser_id, share_name):
#     driver = select_browser(selenium, browser_id)
#     _check_for_item_in_given_list(driver, share_name)
#
#
# @when(parsers.parse('user of {browser_id} selects "{share_name}" share from shares list'))
# @then(parsers.parse('user of {browser_id} selects "{share_name}" share from shares list'))
# def select_share_from_share_list(selenium, browser_id, share_name):
#     driver = select_browser(selenium, browser_id)
#     click_on_element(driver, 'nav.secondary-sidebar ul.shares-list li .truncate', share_name,
#                      'clicking on {:s} share in shares list', ignore_case=False)


@when(parsers.parse('user of {browser_id} should not see metadata record with '
                    'attribute named "{attribute_name}" in metadata panel '
                    'opened for {item_type} named "{item_name}"'))
@then(parsers.parse('user of {browser_id} should not see metadata record with '
                    'attribute named "{attribute_name}" in metadata panel '
                    'opened for {item_type} named "{item_name}"'))
def has_metadata_record_not_appeared(selenium, browser_id, attribute_name,
                                     item_type, item_name):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    Wait(driver, WAIT_FRONTEND).until_not(
        lambda _: _get_metadata_record(panel, attribute_name),
        message='checking there is no metadata with'
                'attribute {attribute}'.format(attribute=attribute_name)
    )


@when(parsers.parse('user of {browser_id} should see metadata record '
                    'with attribute named "{attr_name}" and value "{attr_val}" '
                    'in metadata panel opened for {item_type} named "{item_name}"'))
@then(parsers.parse('user of {browser_id} should see metadata record '
                    'with attribute named "{attr_name}" and value "{attr_val}" '
                    'in metadata panel opened for {item_type} named "{item_name}"'))
def check_if_new_metadata_record_has_appeared(selenium, browser_id, attr_name,
                                              attr_val, item_type, item_name):
    driver = select_browser(selenium, browser_id)
    panel = _get_metadata_panel_for_file(_get_items_from_file_list(driver),
                                         item_name, item_type)
    attr = Wait(driver, WAIT_FRONTEND).until(
        lambda _: _get_metadata_record(panel, attr_name),
        message='searching for metadata record with attribute '
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
    record = _get_metadata_record(files_table, attribute_name)
    Wait(driver, WAIT_FRONTEND).until(
        lambda s: _get_metadata_record(driver, attribute_name),
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
        lambda s: _get_metadata_record(driver, attribute_name),
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
        lambda s: _get_metadata_record(driver, attribute_name, unsaved=True),
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


@then(parsers.parse('user of {browser_id} sees that entered metadata record with attribute '
                    '"{attribute_name}" is red'))
def check_if_entered_metadata_record_is_red(selenium, browser_id, attribute_name):
    driver = select_browser(selenium, browser_id)
    record = _get_metadata_record(driver, attribute_name, unsaved=True)
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
