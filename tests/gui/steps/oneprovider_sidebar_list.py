"""Steps used for sidebar item list handling in various GUI testing scenarios
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.utils.generic import parse_seq, repeat_failed
from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND, MAX_REFRESH_COUNT
from tests.gui.utils.generic import refresh_and_call

from selenium.webdriver.support.ui import WebDriverWait as Wait
from pytest_bdd import parsers, when, then
from pytest_selenium_multi.pytest_selenium_multi import select_browser


def _get_items_from_sidebar_list(driver, ul_type):
    items = driver.find_elements_by_css_selector('ul.{ul_type}-list > li, '
                                                 'ul.{ul_type}-list > li '
                                                 '.first-level '
                                                 '.item-click-area '
                                                 '.item-label'
                                                 ''.format(ul_type=ul_type))
    return {label.text: item for item, label in zip(items[::2], items[1::2])}


def _select_items_from_sidebar_list(driver, browser_id, tmp_memory,
                                    items_names, items_type, items=None):
    items = items if items else _get_items_from_sidebar_list(driver,
                                                             items_type)
    for item_name in parse_seq(items_names):
        item = items.get(item_name)
        if item and 'active' not in item.get_attribute('class'):
            item.click()
            tmp_memory[browser_id][items_type][item_name] = item


def _not_in_sidebar_list(driver, items_names, items_type, items=None):
    items = items if items else _get_items_from_sidebar_list(driver,
                                                             items_type)
    for item_name in parse_seq(items_names):
        item = items.get(item_name)
        if item:
            return False
    return True


@when(parsers.parse('user of {browser_id} selects "{item_name}" '
                    'from {item_type} sidebar list'))
@then(parsers.parse('user of {browser_id} selects "{item_name}" '
                    'from {item_type} sidebar list'))
def select_item_from_sidebar_list(selenium, browser_id, item_name,
                                  item_type, tmp_memory):
    driver = select_browser(selenium, browser_id)
    _select_items_from_sidebar_list(driver, browser_id, tmp_memory,
                                    item_name, item_type)


@when(parsers.parse('user of {browser_id} clicks on settings icon displayed '
                    'for "{item_name}" item on the {item_type} sidebar list'))
@then(parsers.parse('user of {browser_id} clicks on settings icon displayed '
                    'for "{item_name}" item on the {item_type} sidebar list'))
def click_settings_icon_for_item(selenium, browser_id, item_name, item_type):
    driver = select_browser(selenium, browser_id)
    items = _get_items_from_sidebar_list(driver, item_type)
    item = items.get(item_name)
    if not item:
        raise RuntimeError('no {:s} named {:s} found'.format(item_type,
                                                             item_name))

    settings_icon = item.find_element_by_css_selector('.settings-dropdown '
                                                      '.dropdown-toggle')
    driver.execute_script('arguments[0].scrollIntoView();', settings_icon)
    settings_icon.click()

    Wait(driver, WAIT_FRONTEND).until(
        lambda _: settings_icon.get_attribute('aria-expanded') == 'true',
        message='waiting for settings dropdown for {name} on {type} to appear'
                ''.format(name=item_name, type=item_type)
    )


@when(parsers.parse('user of {browser_id} clicks on the "{option_name}" item '
                    'in settings dropdown for {item_type} named "{item_name}"'))
@then(parsers.parse('user of {browser_id} clicks on the "{option_name}" item '
                    'in settings dropdown for {item_type} named "{item_name}"'))
def click_on_item_in_settings_dropdown(selenium, browser_id, option_name,
                                       item_name, item_type):
    driver = select_browser(selenium, browser_id)
    items = _get_items_from_sidebar_list(driver, '{}s'.format(item_type))
    item = items.get(item_name)
    if not item:
        raise RuntimeError('no {:s} named {:s} found'.format(item_type,
                                                             item_name))

    options = item.find_elements_by_css_selector('.settings-dropdown '
                                                 '.dropdown-menu-settings '
                                                 'li.clickable .item-label')
    option_name = option_name.lower()
    for option in options:
        if option.text.lower() == option_name:
            option.click()
            break
    else:
        raise RuntimeError('no option named {:s} found'.format(option_name))


# TODO remove refresh after gui will become more responsive
@when(parsers.re(r'user of (?P<browser_id>.*?) sees that (?P<item_list>.*?) '
                 r'(has|have) disappeared from (?P<item_type>.*?) sidebar list'))
@then(parsers.re(r'user of (?P<browser_id>.*?) sees that (?P<item_list>.*?) '
                 r'(has|have) disappeared from (?P<item_type>.*?) sidebar list'))
@when(parsers.re(r'user of (?P<browser_id>.*?) does not see (?P<item_list>.*?) '
                 r'in (?P<item_type>.*?) sidebar list'))
@then(parsers.re(r'user of (?P<browser_id>.*?) does not see (?P<item_list>.*?) '
                 r'in (?P<item_type>.*?) sidebar list'))
def is_not_present_in_sidebar_list(selenium, browser_id, item_list, item_type):
    driver = select_browser(selenium, browser_id)
    Wait(driver, MAX_REFRESH_COUNT * WAIT_BACKEND).until(
        lambda d: refresh_and_call(d, _not_in_sidebar_list,
                                   item_list, item_type),
        message='searching for absence of {item} '
                'on {list} list'.format(item=item_list,
                                        list=item_type)
    )


# TODO remove refresh after gui will become more responsive
@when(parsers.re(r'user of (?P<browser_id>.*?) sees that (?P<item_list>.*?) '
                 r'(has|have) appeared on (?P<item_type>.*?) sidebar list'))
@then(parsers.re(r'user of (?P<browser_id>.*?) sees that (?P<item_list>.*?) '
                 r'(has|have) appeared on (?P<item_type>.*?) sidebar list'))
@when(parsers.re(r'user of (?P<browser_id>.*?) sees (?P<item_list>.*?) '
                 r'in (?P<item_type>.*?) sidebar list'))
@then(parsers.re(r'user of (?P<browser_id>.*?) sees (?P<item_list>.*?) '
                 r'in (?P<item_type>.*?) sidebar list'))
def is_present_in_sidebar_list(selenium, browser_id, item_list,
                               item_type, tmp_memory):
    driver = select_browser(selenium, browser_id)
    Wait(driver, MAX_REFRESH_COUNT * WAIT_BACKEND).until(
        lambda d: refresh_and_call(d, lambda _, n, t: not _not_in_sidebar_list(d, n, t),
                                   tmp_memory[browser_id]['gen_str']
                                   if item_list == 'a new item'
                                   else item_list,
                                   item_type),
        message='searching for presence of {item} '
                'on {list} list'.format(item=item_list,
                                        list=item_type)
    )


@when(parsers.parse('user of {browser_id} clicks on the "{btn_name}" '
                    'button in sidebar list\'s header'))
@then(parsers.parse('user of {browser_id} clicks on the "{btn_name}" '
                    'button in sidebar list\'s header'))
def click_on_button_in_sidebar_header(selenium, browser_id, btn_name):
    driver = select_browser(selenium, browser_id)
    buttons = driver.find_elements_by_css_selector('.secondary-sidebar-header '
                                                   'figure.icon')
    btn_name = btn_name.lower()
    for btn in buttons:
        if btn.text.lower() == btn_name:
            btn.click()
            break
    else:
        raise RuntimeError('no button named {:s} found'.format(btn_name))


@when(parsers.parse('user of {browser_id} sees that submenu for '
                    '{item_type} named "{item_name}" has appeared'))
@then(parsers.parse('user of {browser_id} sees that submenu for '
                    '{item_type} named "{item_name}" has appeared'))
def has_submenu_appeared(browser_id, item_type, item_name, tmp_memory):
    item = tmp_memory[browser_id]['{}s'.format(item_type)][item_name]

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def assert_submenu_appeared(elem, elem_type, elem_name):
        assert elem.find_element_by_css_selector('ul.submenu'), \
            'submenu for {} named {} not found'.format(elem_type, elem_name)

    assert_submenu_appeared(item, item_type, item_name)
