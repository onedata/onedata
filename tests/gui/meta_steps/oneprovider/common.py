"""Metasteps for tests of Oneprovider
"""

__author__ = "Michal Stanisz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


import yaml
from pytest_bdd import when, then, parsers
from tests.gui.utils.oneprovider_gui import OPLoggedIn as op_page
from tests.gui.utils.common.modals import Modals as modals
from tests.gui.utils.generic import repeat_failed
from tests.gui.steps.oneprovider.data_tab import *
from tests.gui.steps.oneprovider_common import *
from tests.gui.steps.common import *
from tests.gui.steps.modal import *
from tests.gui.steps.oneprovider.file_browser import *
from tests.gui.steps.oneprovider_file_list import *
from tests.gui.steps.oneprovider_transfers import *
 

@when(parsers.re('user of (?P<browser_id>.*) changes current space in data tab'
                 ' to "(?P<space_name>.*)"'))
@then(parsers.re('user of (?P<browser_id>.*) changes current space in data tab'
                 ' to "(?P<space_name>.*)"'))
def change_space_in_data_tab(selenium, browser_id, space_name, tmp_memory):
    wt_click_on_the_given_main_menu_tab(selenium, browser_id, 'data')
    change_space_view_in_data_tab_in_op(selenium, browser_id, space_name, op_page)
    is_displayed_breadcrumbs_in_data_tab_in_op_correct(selenium, browser_id, 
                                                       space_name, op_page)
    assert_file_browser_in_data_tab_in_op(selenium, browser_id, op_page, 
                                          tmp_memory)


@when(parsers.re('user of (?P<browser_id>.*) uploads file "(?P<file_name>.*)"'))
@then(parsers.re('user of (?P<browser_id>.*) uploads file "(?P<file_name>.*)"'))
def upload_file(selenium, browser_id, file_name, tmp_memory):
    assert_file_browser_in_data_tab_in_op(selenium, browser_id, op_page, 
                                          tmp_memory)
    upload_file_to_cwd_in_data_tab(selenium, browser_id, file_name, op_page)
    notify_visible_with_text(selenium, browser_id, 'info', '.*[Cc]ompleted upload') 
    assert_items_presence_in_file_browser(browser_id, file_name, tmp_memory)


@when(parsers.re('user of (?P<browser_id>.*) replicates "(?P<name>.*)" to '
                 'provider "(?P<provider>.*)"'))
@then(parsers.re('user of (?P<browser_id>.*) replicates "(?P<name>.*)" to '
                 'provider "(?P<provider>.*)"'))
def meta_replicate_item(selenium, browser_id, name, tmp_memory, provider):
    tooltip = 'Show data distribution'
    modal_name = 'Data distribution'
    select_files_from_file_list(selenium, browser_id, name)
    click_tooltip_from_toolbar_in_data_tab_in_op(selenium, browser_id, tooltip, 
                                                 op_page)
    wait_for_modal_to_appear(selenium, browser_id, modal_name, tmp_memory)
    replicate_item(selenium, browser_id, provider) 
    click_on_confirmation_btn_in_modal(selenium, browser_id, 'Close', tmp_memory)
    wait_for_modal_to_disappear(selenium, browser_id, tmp_memory)
    deselect_files_from_file_list(selenium, browser_id, name)


@when(parsers.re('user of (?P<browser_id>.*) migrates "(?P<name>.*)" from '
                 'provider "(?P<source>.*)" to provider "(?P<target>.*)"'))
@then(parsers.re('user of (?P<browser_id>.*) migrates "(?P<name>.*)" from '
                 'provider "(?P<source>.*)" to provider "(?P<target>.*)"'))
def meta_migrate_item(selenium, browser_id, name, tmp_memory, source, 
                      target):
    tooltip = 'Show data distribution'
    modal_name = 'Data distribution'
    select_files_from_file_list(selenium, browser_id, name)
    click_tooltip_from_toolbar_in_data_tab_in_op(selenium, browser_id, tooltip, 
                                                 op_page)
    wait_for_modal_to_appear(selenium, browser_id, modal_name, tmp_memory)
    migrate_item(selenium, browser_id, source, target) 
    click_on_confirmation_btn_in_modal(selenium, browser_id, 'Close', tmp_memory)
    wait_for_modal_to_disappear(selenium, browser_id, tmp_memory)
    deselect_files_from_file_list(selenium, browser_id, name)


@when(parsers.re('user of (?P<browser_id>.*) removes "(?P<name>.*)" in '
                 'file browser'))
@then(parsers.re('user of (?P<browser_id>.*) removes "(?P<name>.*)" in '
                 'file browser'))
def remove_item(selenium, browser_id, name, tmp_memory):
    tooltip = 'Remove element'
    modal_name = 'Remove files'
    assert_file_browser_in_data_tab_in_op(selenium, browser_id, op_page, 
                                          tmp_memory)
    select_files_from_file_list(selenium, browser_id, name)
    click_tooltip_from_toolbar_in_data_tab_in_op(selenium, browser_id, tooltip, 
                                                 op_page)
    wait_for_modal_to_appear(selenium, browser_id, modal_name, tmp_memory)
    click_on_confirmation_btn_in_modal(selenium, browser_id, 'Yes', tmp_memory)
    wait_for_modal_to_disappear(selenium, browser_id, tmp_memory)
    assert_items_absence_in_file_browser(selenium, browser_id, name, tmp_memory)
    

@when(parsers.re('user of (?P<browser_id>.*) creates directory "(?P<name>.*)"'))
@then(parsers.re('user of (?P<browser_id>.*) creates directory "(?P<name>.*)"'))
def create_directory(selenium, browser_id, name, tmp_memory):
    tooltip = 'Create directory'
    modal_name = 'New directory'
    assert_file_browser_in_data_tab_in_op(selenium, browser_id, op_page, 
                                          tmp_memory)
    click_tooltip_from_toolbar_in_data_tab_in_op(selenium, browser_id, tooltip, 
                                                 op_page)
    wait_for_modal_to_appear(selenium, browser_id, modal_name, tmp_memory)
    activate_input_box_in_modal(browser_id, '', tmp_memory)
    type_string_into_active_element(selenium, browser_id, name)
    click_on_confirmation_btn_in_modal(selenium, browser_id, 'Create', 
                                       tmp_memory)
    wait_for_modal_to_disappear(selenium, browser_id, tmp_memory)
    assert_items_presence_in_file_browser(browser_id, name, tmp_memory)


@when(parsers.re('user of (?P<browser_id>.*) sees file chunks for file '
                 '"(?P<file_name>.*)" as follows:\n(?P<desc>(.|\s)*)'))
@then(parsers.re('user of (?P<browser_id>.*) sees file chunks for file '
                 '"(?P<file_name>.*)" as follows:\n(?P<desc>(.|\s)*)'))
def assert_file_chunks(selenium, browser_id, file_name, desc, tmp_memory):
    tooltip = 'Show data distribution'
    modal_name = 'Data distribution'
    assert_file_browser_in_data_tab_in_op(selenium, browser_id, op_page, 
                                          tmp_memory)
    select_files_from_file_list(selenium, browser_id, file_name)
    click_tooltip_from_toolbar_in_data_tab_in_op(selenium, browser_id, tooltip, 
                                                 op_page)
    wait_for_modal_to_appear(selenium, browser_id, modal_name, tmp_memory)
    desc = yaml.load(desc)
    for provider, chunks in desc.items():
        if chunks == 'never synchronized':
            assert_item_never_synchronized(selenium, browser_id, provider)
        elif chunks == 'entirely empty':
            assert_provider_chunk_in_data_distribution_empty(selenium, 
                                                             browser_id, 
                                                             provider, modals)
        elif chunks == 'entirely filled':
            assert_provider_chunk_in_data_distribution_filled(selenium, 
                                                              browser_id, 
                                                              provider, modals)
    click_on_confirmation_btn_in_modal(selenium, browser_id, 'Close', tmp_memory)
    wait_for_modal_to_disappear(selenium, browser_id, tmp_memory)
    deselect_files_from_file_list(selenium, browser_id, file_name)
