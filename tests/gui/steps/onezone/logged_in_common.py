"""Steps for features of Onezone login page.
"""

from pytest_bdd import given, when, then, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser

from tests.gui.conftest import WAIT_BACKEND, SELENIUM_IMPLICIT_WAIT
from tests.gui.utils.generic import repeat_failed, implicit_wait
from tests.utils.acceptance_utils import list_parser


__author__ = "Jakub Liput, Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def _expand_oz_panel(oz_page, driver, panel):
    oz_page(driver)[panel].expand()


@given(parsers.re(r'users? of (?P<browser_id_list>.*) expanded the '
                  r'"(?P<panel_name>.*)" Onezone sidebar panel'))
def g_expand_oz_panel(selenium, browser_id_list, panel_name, oz_page):
    for browser_id in list_parser(browser_id_list):
        driver = select_browser(selenium, browser_id)
        _expand_oz_panel(oz_page, driver, panel_name)


@when(parsers.re(r'users? of (?P<browser_id_list>.*) expands? the '
                 r'"(?P<panel_name>.*)" Onezone sidebar panel'))
@then(parsers.re(r'users? of (?P<browser_id_list>.*) expands? the '
                 r'"(?P<panel_name>.*)" Onezone sidebar panel'))
def wt_expand_oz_panel(selenium, browser_id_list, panel_name, oz_page):
    for browser_id in list_parser(browser_id_list):
        driver = select_browser(selenium, browser_id)
        _expand_oz_panel(oz_page, driver, panel_name)


@when(parsers.re(r'user of (?P<browser_id>.+?) clicks on (?P<btn>confirm|cancel) '
                 r'button displayed next to active edit box'))
@then(parsers.re(r'user of (?P<browser_id>.+?) clicks on (?P<btn>confirm|cancel) '
                 r'button displayed next to active edit box'))
def click_on_btn_for_edit_box(browser_id, btn, tmp_memory):
    edit_box = tmp_memory[browser_id]['edit_box']
    action = getattr(edit_box, btn)
    action()


@when(parsers.parse('user of {browser_id} types "{text}" in active edit box'))
@then(parsers.parse('user of {browser_id} types "{text}" in active edit box'))
def type_text_into_active_edit_box(browser_id, text, tmp_memory):
    edit_box = tmp_memory[browser_id]['edit_box']
    edit_box.value = text


@when(parsers.re(r'user of (?P<browser_id>.+?) clicks on '
                 r'"(?P<btn>Create new access token)" button in expanded '
                 r'"(?P<oz_panel>ACCESS TOKENS)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) clicks on '
                 r'"(?P<btn>Create new access token)" button in expanded '
                 r'"(?P<oz_panel>ACCESS TOKENS)" Onezone panel'))
@when(parsers.re(r'user of (?P<browser_id>.+?) clicks on '
                 r'"(?P<btn>Create new space|Join space)" button in expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) clicks on '
                 r'"(?P<btn>Create new space|Join space)" button in expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def click_on_btn_in_oz_panel(selenium, browser_id, btn, oz_panel, oz_page):
    driver = select_browser(selenium, browser_id)
    action = getattr(oz_page(driver)[oz_panel], btn.lower().replace(' ', '_'))
    action()


@when(parsers.re(r'user of (?P<browser_id>.+?) sees that there is '
                 r'(?P<item_type>provider) named "(?P<item_name>.+?)" '
                 r'in expanded "(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that there is '
                 r'(?P<item_type>provider) named "(?P<item_name>.+?)" '
                 r'in expanded "(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@when(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<item_type>provider) '
                 r'named "(?P<item_name>.+?)" has appeared in expanded '
                 r'"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<item_type>provider) '
                 r'named "(?P<item_name>.+?)" has appeared in expanded '
                 r'"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@when(parsers.re(r'user of (?P<browser_id>.+?) sees that there is '
                 r'(?P<item_type>space) named "(?P<item_name>.+?)" in expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that there is '
                 r'(?P<item_type>space) named "(?P<item_name>.+?)" in expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@when(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<item_type>space) '
                 r'named "(?P<item_name>.+?)" has appeared in expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<item_type>space) '
                 r'named "(?P<item_name>.+?)" has appeared in expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def assert_there_is_item_named_in_oz_panel_list(selenium, browser_id, item_type,
                                                item_name, oz_panel, oz_page):
    driver = select_browser(selenium, browser_id)
    items = getattr(oz_page(driver)[oz_panel], '{}s'.format(item_type))
    assert item_name in items, \
        'no {} named "{}" found in {} oz panel'.format(item_type, item_name,
                                                       oz_panel)


@when(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<item_type>provider) '
                 r'named "(?P<item_name>.+?)" has disappeared from expanded '
                 r'"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<item_type>provider) '
                 r'named "(?P<item_name>.+?)" has disappeared from expanded '
                 r'"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@when(parsers.re(r'user of (?P<browser_id>.+?) sees that there is no '
                 r'(?P<item_type>provider) named "(?P<item_name>.+?)" '
                 r'in expanded "(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that there is no '
                 r'(?P<item_type>provider) named "(?P<item_name>.+?)" '
                 r'in expanded "(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@when(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<item_type>space) '
                 r'named "(?P<item_name>.+?)" has disappeared from expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<item_type>space) '
                 r'named "(?P<item_name>.+?)" has disappeared from expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@when(parsers.re(r'user of (?P<browser_id>.+?) sees that there is no '
                 r'(?P<item_type>space) named "(?P<item_name>.+?)" in expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that there is no '
                 r'(?P<item_type>space) named "(?P<item_name>.+?)" in expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def assert_there_is_no_item_named_in_oz_panel_list(selenium, browser_id,
                                                   item_type, item_name,
                                                   oz_panel, oz_page):
    driver = select_browser(selenium, browser_id)
    items = getattr(oz_page(driver)[oz_panel], '{}s'.format(item_type))
    assert item_name not in items, \
        '{} named "{}" found in {} oz panel while it should not be ' \
        'found'.format(item_type, item_name, oz_panel)


@when(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<counter_type>space)s '
                 r'counter for (?P<item_type>provider) named "(?P<item_name>.+?)" '
                 r'displays (?P<number>\d+) in expanded '
                 r'"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<counter_type>space)s '
                 r'counter for (?P<item_type>provider) named "(?P<item_name>.+?)" '
                 r'displays (?P<number>\d+) in expanded '
                 r'"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@when(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<counter_type>provider)s '
                 r'counter for (?P<item_type>space) named "(?P<item_name>.+?)" '
                 r'displays (?P<number>\d+) in expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<counter_type>provider)s '
                 r'counter for (?P<item_type>space) named "(?P<item_name>.+?)" '
                 r'displays (?P<number>\d+) in expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def assert_item_counter_match_given_num(selenium, browser_id, counter_type,
                                        item_type, item_name, number,
                                        oz_panel, oz_page):
    driver = select_browser(selenium, browser_id)
    items = getattr(oz_page(driver)[oz_panel], '{}s'.format(item_type))
    item = items[item_name]
    item_counter = int(getattr(item, '{}s_count'.format(counter_type)))

    msg = 'expected {counter_type}s number {num} does not match ' \
          'displayed {counter_type}s counter {displayed} ' \
          'for {type} named "{name}"'
    assert item_counter == int(number), msg.format(type=item_type,
                                                   name=item_name,
                                                   counter_type=counter_type,
                                                   displayed=item_counter,
                                                   num=number)


@when(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<item_type>provider) '
                 r'named "(?P<item_name>.+?)" is set as home (?P=item_type) '
                 r'in expanded "(?P<oz_panel>GO TO YOUR FILES)" '
                 r'Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<item_type>provider) '
                 r'named "(?P<item_name>.+?)" is set as home (?P=item_type) '
                 r'in expanded "(?P<oz_panel>GO TO YOUR FILES)" '
                 r'Onezone panel'))
@when(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<item_type>space) '
                 r'named "(?P<item_name>.+?)" is set as home (?P=item_type) '
                 r'in expanded "(?P<oz_panel>DATA SPACE MANAGEMENT)" '
                 r'Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<item_type>space) '
                 r'named "(?P<item_name>.+?)" is set as home (?P=item_type) '
                 r'in expanded "(?P<oz_panel>DATA SPACE MANAGEMENT)" '
                 r'Onezone panel'))
@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def assert_item_is_home_item_in_oz_panel(selenium, browser_id, item_type,
                                         item_name, oz_panel, oz_page):
    driver = select_browser(selenium, browser_id)
    items = getattr(oz_page(driver)[oz_panel], '{}s'.format(item_type))
    item = items[item_name]

    err_msg = '{type} named "{name}" is not set as home while it should be ' \
              'in {panel} oz panel'
    assert item.is_home(), err_msg.format(type=item_type, name=item_name,
                                          panel=oz_panel)


@when(parsers.re(r'user of (?P<browser_id>.+?) sets (?P<item_type>provider) '
                 r'named "(?P<item_name>.+?)" as home by clicking on '
                 r'home outline in that (?P=item_type) record in expanded '
                 r'"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sets (?P<item_type>provider) '
                 r'named "(?P<item_name>.+?)" as home by clicking on '
                 r'home outline in that (?P=item_type) record in expanded '
                 r'"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@when(parsers.re(r'user of (?P<browser_id>.+?) sets (?P<item_type>space) named '
                 r'"(?P<item_name>.+?)" as home by clicking on home outline '
                 r'in that (?P=item_type) record in expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sets (?P<item_type>space) named '
                 r'"(?P<item_name>.+?)" as home by clicking on home outline '
                 r'in that (?P=item_type) record in expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def set_given_item_as_home_by_clicking_on_home_outline(selenium, browser_id,
                                                       item_type, item_name,
                                                       oz_panel, oz_page):
    driver = select_browser(selenium, browser_id)
    items = getattr(oz_page(driver)[oz_panel], '{}s'.format(item_type))
    item = items[item_name]
    with implicit_wait(driver, 0.2, SELENIUM_IMPLICIT_WAIT):
        item.set_as_home()


@when(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<counter_type>space)s '
                 r'counter for "(?P<item_name>.+?)" match number of displayed '
                 r'supported spaces in expanded submenu '
                 r'of given (?P<item_type>provider) in expanded '
                 r'"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<counter_type>space)s '
                 r'counter for "(?P<item_name>.+?)" match number of displayed '
                 r'supported spaces in expanded submenu '
                 r'of given (?P<item_type>provider) in expanded '
                 r'"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@when(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<counter_type>provider)s '
                 r'counter for "(?P<item_name>.+?)" match number of displayed '
                 r'supporting providers in expanded submenu '
                 r'of given (?P<item_type>space) in expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<counter_type>provider)s '
                 r'counter for "(?P<item_name>.+?)" match number of displayed '
                 r'supporting providers in expanded submenu '
                 r'of given (?P<item_type>space) in expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def assert_number_of_items_match_items_counter(selenium, browser_id, item_name,
                                               item_type, counter_type, oz_panel,
                                               oz_page):
    driver = select_browser(selenium, browser_id)
    items = getattr(oz_page(driver)[oz_panel], '{}s'.format(item_type))
    item = items[item_name]
    subitems = getattr(item, '{}s'.format(counter_type))
    counter = int(getattr(item, '{}s_count'.format(counter_type)))

    err_msg = '{type}s counter number {counter} does not match displayed number ' \
              'of {type}s {list_len}'
    assert counter == subitems.count(), err_msg.format(type=counter_type,
                                                       counter=counter,
                                                       list_len=subitems.count())


@when(parsers.re(r'user of (?P<browser_id>.+?) expands submenu of '
                 r'(?P<item_type>provider) named "(?P<item_name>.+?)" by '
                 r'clicking on cloud in provider record in expanded '
                 r'"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) expands submenu of '
                 r'(?P<item_type>provider) named "(?P<item_name>.+?)" by '
                 r'clicking on cloud in provider record in expanded '
                 r'"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@when(parsers.re(r'user of (?P<browser_id>.+?) expands submenu of '
                 r'(?P<item_type>space) named "(?P<item_name>.+?)" '
                 r'by clicking on space record in expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) expands submenu of '
                 r'(?P<item_type>space) named "(?P<item_name>.+?)" '
                 r'by clicking on space record in expanded '
                 r'"(?P<oz_panel>DATA SPACE MANAGEMENT)" Onezone panel'))
@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def expand_items_submenu_in_oz_panel(selenium, browser_id, item_type,
                                     item_name, oz_panel, oz_page):
    driver = select_browser(selenium, browser_id)
    items = getattr(oz_page(driver)[oz_panel], '{}s'.format(item_type))
    item = items[item_name]
    item.expand()
    err_msg = 'submenu for {type} named "{name}" has not been expanded'
    assert item.is_expanded(), err_msg.format(type=item_type, name=item_name)


@when(parsers.re(r'user of (?P<browser_id>.+?) sees that '
                 r'(?P<subitem_type>space) named "(?P<subitem_name>.+?)" '
                 r'in submenu of (?P<item_type>provider) named '
                 r'"(?P<item_name>.+?)" in expanded '
                 r'"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel '
                 r'is set as home'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that '
                 r'(?P<subitem_type>space) named "(?P<subitem_name>.+?)" '
                 r'in submenu of (?P<item_type>provider) named '
                 r'"(?P<item_name>.+?)" in expanded '
                 r'"(?P<oz_panel>GO TO YOUR FILES)" Onezone panel '
                 r'is set as home'))
@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def assert_subitem_is_set_as_home(selenium, browser_id, subitem_type,
                                  subitem_name, item_type, item_name,
                                  oz_panel, oz_page):
    driver = select_browser(selenium, browser_id)
    items = getattr(oz_page(driver)[oz_panel], '{}s'.format(item_type))
    item = items[item_name]
    subitems = getattr(item, '{}s'.format(subitem_type))
    subitem = subitems[subitem_name]

    err_msg = '{subtype} named "{subitem}" is not set as home for {type} ' \
              'named "{name}" while it should be'
    assert subitem.is_home(), err_msg.format(subtype=subitem_type,
                                             subitem=subitem_name,
                                             type=item_type,
                                             name=item_name)


@when(parsers.re(r'user of (?P<browser_id>.+?) sees that there is '
                 r'(?P<subitem_type>provider) named "(?P<subitem_name>.+?)" '
                 r'in submenu of (?P<item_type>space) named "(?P<item_name>.+?)" '
                 r'in expanded "(?P<oz_panel>DATA SPACE MANAGEMENT)" '
                 r'Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that there is '
                 r'(?P<subitem_type>provider) named "(?P<subitem_name>.+?)" '
                 r'in submenu of (?P<item_type>space) named "(?P<item_name>.+?)" '
                 r'in expanded "(?P<oz_panel>DATA SPACE MANAGEMENT)" '
                 r'Onezone panel'))
@when(parsers.re(r'user of (?P<browser_id>.+?) sees that there is '
                 r'(?P<subitem_type>space) named "(?P<subitem_name>.+?)" '
                 r'in submenu of (?P<item_type>provider) named "(?P<item_name>.+?)" '
                 r'in expanded "(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that there is '
                 r'(?P<subitem_type>space) named "(?P<subitem_name>.+?)" '
                 r'in submenu of (?P<item_type>provider) named "(?P<item_name>.+?)" '
                 r'in expanded "(?P<oz_panel>GO TO YOUR FILES)" Onezone panel'))
@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def assert_item_in_submenu_of_item_in_oz_panel(selenium, browser_id, subitem_type,
                                               subitem_name, item_type, item_name,
                                               oz_panel, oz_page):
    driver = select_browser(selenium, browser_id)
    items = getattr(oz_page(driver)[oz_panel], '{}s'.format(item_type))
    item = items[item_name]
    subitems = getattr(item, '{}s'.format(subitem_type))
    assert subitem_name in subitems, \
        'no "{}" found in subitems of "{}" in {}'.format(subitem_name,
                                                         item_name, oz_panel)
