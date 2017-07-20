"""This module contains gherkin steps to run acceptance tests featuring
storages management in onepanel web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


from pytest_bdd import when, then, parsers

from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed, transform


@when(parsers.parse('user of {browser_id} selects {storage_type} from storage '
                    'selector in storages page in Onepanel'))
@then(parsers.parse('user of {browser_id} selects {storage_type} from storage '
                    'selector in storages page in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_select_storage_type_in_storage_page_op_panel(selenium, browser_id,
                                                    storage_type, onepanel):
    storage_selector = (onepanel(selenium[browser_id]).content.storages.
                        form.storage_selector)
    storage_selector.expand()
    storage_selector.options[storage_type].click()


@when(parsers.re('user of (?P<browser_id>.*?) types "(?P<text>.*?)" to '
                 '(?P<input_box>.*?) field in (?P<form>POSIX) form '
                 'in storages page in Onepanel'))
@then(parsers.re('user of (?P<browser_id>.*?) types "(?P<text>.*?)" to '
                 '(?P<input_box>.*?) field in (?P<form>POSIX) form '
                 'in storages page in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_text_to_in_box_in_storages_page_op_panel(selenium, browser_id,
                                                     text, form, onepanel,
                                                     input_box):
    form = getattr(onepanel(selenium[browser_id]).content.storages.form,
                   transform(form))
    setattr(form, transform(input_box), text)


@when(parsers.parse('user of {browser_id} clicks on Add button in add storage '
                    'form in storages page in Onepanel'))
@then(parsers.parse('user of {browser_id} clicks on Add button in add storage '
                    'form in storages page in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_click_on_add_btn_in_storage_add_form_in_storage_page(selenium,
                                                            browser_id,
                                                            onepanel):
    onepanel(selenium[browser_id]).content.storages.form.add()


@when(parsers.parse('user of {browser_id} expands "{storage}" record on '
                    'storages list in storages page in Onepanel'))
@then(parsers.parse('user of {browser_id} expands "{storage}" record on '
                    'storages list in storages page in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_expand_storage_item_in_storages_page_op_panel(selenium, browser_id,
                                                     storage, onepanel):
    onepanel(selenium[browser_id]).content.storages.storages[storage].expand()


@when(parsers.re('user of (?P<browser_id>.*?) sees that "(?P<storage>.*?)" '
                 '(?P<attr>Storage type|Mount point) is (?P<val>.*?) '
                 'in storages page in Onepanel'))
@then(parsers.re('user of (?P<browser_id>.*?) sees that "(?P<storage>.*?)" '
                 '(?P<attr>Storage type|Mount point) is (?P<val>.*?) '
                 'in storages page in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_assert_storage_attr_in_storages_page_op_panel(selenium, browser_id,
                                                     storage, attr, val,
                                                     onepanel):
    storages = onepanel(selenium[browser_id]).content.storages.storages
    displayed_val = getattr(storages[storage], transform(attr)).lower()
    assert displayed_val == val.lower(), \
        'got {} as storage\'s {} instead of expected {}'.format(displayed_val,
                                                                attr, val)
