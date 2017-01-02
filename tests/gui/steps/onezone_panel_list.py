"""Steps for features of Onezone login page.
"""


from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from tests.gui.steps.onezone_logged_in_common import panel_to_css
from tests.utils.acceptance_utils import list_parser

from selenium.webdriver.support.ui import WebDriverWait as Wait

from pytest_bdd import given, when, then, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


TOOL_TO_CSS = {'home outline icon': 'oneicon-home-outline',
               'home icon': 'oneicon-home'}


def get_items_from_oz_panel_list(driver, panel, item_type):
    panel = panel_to_css[panel.lower()]
    css_sel = '{0} .{1}s-accordion-item, {0} .{1}s-accordion-item .{1}-header'
    items = driver.find_elements_by_css_selector(css_sel.format(panel,
                                                                item_type))
    return {label.text: item for label, item in zip(items[1::2], items[::2])}


def get_item_from_oz_panel_list(driver, item_name, item_type, panel):
    items = get_items_from_oz_panel_list(driver, panel, item_type)
    return items.get(item_name, None)


def get_submenu_for_item_from_oz_panel_list(driver, item_name, item_type, panel):
    item = get_item_from_oz_panel_list(driver, item_name, item_type, panel)
    css_sel = '[id*=collapse-{}]'.format(item_type)
    return item.find_element_by_css_selector(css_sel) if item else None


def _is_in_panel_list(driver, panel, item_type, items):
    all_items = get_items_from_oz_panel_list(driver, panel, item_type)
    return all(item in all_items for item in items)


def _is_not_in_panel_list(driver, panel, item_type, items):
    all_items = get_items_from_oz_panel_list(driver, panel, item_type)
    return all(item not in all_items for item in items)


def _click_on_list_item(driver, item_name, item_type, panel):
    Wait(driver, WAIT_FRONTEND).until(
        lambda d: get_item_from_oz_panel_list(d, item_name, item_type, panel),
        message='clicking on {} named "{}" from uncollapsed {} panel'
                ''.format(item_type, item_name, panel)
    ).click()


def _is_marked_as_home(driver, item_name, item_type, panel):
    item = Wait(driver, WAIT_BACKEND).until(
        lambda d: get_item_from_oz_panel_list(d, item_name, item_type, panel),
        message='no {} named "{}" found'.format(item_type, item_name)
    )

    err_msg = '"{}" is not marked as home {}'.format(item_name, item_type)
    assert item.find_element_by_css_selector('.oneicon-home'), err_msg
    assert item.find_element_by_css_selector('.oneicon-{}-home'
                                             ''.format(item_type)), err_msg


@when(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<items>.+?) '
                 r'(has|have) disappeared from (?P<item_type>.+?)s list '
                 r'in expanded "(?P<panel>.+?)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that (?P<items>.+?) '
                 r'(has|have) disappeared from (?P<item_type>.+?)s list '
                 r'in expanded "(?P<panel>.+?)" Onezone panel'))
@when(parsers.re(r'user of (?P<browser_id>.*?) does not see (?P<items>.*?) '
                 r'on (?P<item_type>.*?)s list in expanded '
                 r'"(?P<panel>.+?)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.*?) does not see (?P<items>.*?) '
                 r'on (?P<item_type>.*?)s list in expanded '
                 r'"(?P<panel>.+?)" Onezone panel'))
def is_not_present_in_panel_list(selenium, browser_id, items,
                                 item_type, panel, tmp_memory):
    driver = select_browser(selenium, browser_id)
    items = tmp_memory[browser_id]['gen_str'] if items == 'new item' else items
    Wait(driver, WAIT_BACKEND).until(
        lambda d: _is_not_in_panel_list(driver, panel, item_type,
                                        list_parser(items)),
        message='checking for absence of "{items}" on {list_type} list'
                ''.format(items=items, list_type=item_type)
    )


@when(parsers.re(r'user of (?P<browser_id>.*?) sees that (?P<items>.*?) '
                 r'(has|have) appeared on (?P<item_type>.*?)s list '
                 r'in expanded "(?P<panel>.+?)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.*?) sees that (?P<items>.*?) '
                 r'(has|have) appeared on (?P<item_type>.*?)s list '
                 r'in expanded "(?P<panel>.+?)" Onezone panel'))
@when(parsers.re(r'user of (?P<browser_id>.*?) sees on (?P<item_type>.*?)s '
                 r'list in expanded "(?P<panel>.+?)" Onezone panel  '
                 r'following items?: (?P<items>.*?)'))
@when(parsers.re(r'user of (?P<browser_id>.*?) sees on (?P<item_type>.*?)s '
                 r'list in expanded "(?P<panel>.+?)" Onezone panel  '
                 r'following items?: (?P<items>.*?)'))
def is_present_in_panel_list(selenium, browser_id, items,
                             item_type, panel, tmp_memory):
    driver = select_browser(selenium, browser_id)
    items = tmp_memory[browser_id]['gen_str'] if items == 'new item' else items
    Wait(driver, WAIT_BACKEND).until(
        lambda d: _is_in_panel_list(driver, panel, item_type,
                                    list_parser(items)),
        message='checking for presence of "{items}" on {list_type} list'
                ''.format(items=items, list_type=item_type)
    )


@given(parsers.parse('user of {browser_id} clicked on item named "{item_name}" '
                     'in {item_type}s list in expanded "{panel}" Onezone panel'))
def g_click_on_list_item_in_expanded_panel(selenium, browser_id, item_name,
                                           item_type, panel):
    driver = select_browser(selenium, browser_id)
    _click_on_list_item(driver, item_name, item_type, panel)


@when(parsers.parse('user of {browser_id} clicks on item named "{item_name}" '
                    'in {item_type}s list in expanded "{panel}" Onezone panel'))
@when(parsers.parse('user of {browser_id} clicks on item named "{item_name}" '
                    'in {item_type}s list in expanded "{panel}" Onezone panel'))
def wt_click_on_list_item_in_expanded_panel(selenium, browser_id, item_name,
                                            item_type, panel):
    driver = select_browser(selenium, browser_id)
    _click_on_list_item(driver, item_name, item_type, panel)


@when(parsers.parse('user of {browser_id} clicks on {tool_name} in item row '
                    'for item named "{item_name}" in {item_type}s list in '
                    'expanded "{panel}" Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on {tool_type} in item row '
                    'for item named "{item_name}" in {item_type}s list in '
                    'expanded "{panel}" Onezone panel'))
def click_on_tool_in_item_row(selenium, browser_id, tool_name, item_name,
                              item_type, panel):
    driver = select_browser(selenium, browser_id)
    css_sel = '.{}'.format(TOOL_TO_CSS[tool_name])
    err_msg = 'clicking on {tool} for item named "{name}" in {type} list'

    Wait(driver, WAIT_BACKEND).until(
        lambda d: get_item_from_oz_panel_list(d, item_name, item_type, panel),
        message=err_msg.format(tool=tool_name, name=item_name, type=item_type)
    ).find_element_by_css_selector(css_sel).click()


@given(parsers.re('user of (?P<browser_id>.+?) seen that item named '
                  '"(?P<item_name>.+?)" in (?P<item_type>.+?)s list '
                  'in expanded "(?P<panel>.+?)" Onezone panel '
                  'was marked as home (?P=item_type)'))
def g_is_marked_as_home_item(selenium, browser_id, item_name,
                             item_type, panel):
    driver = select_browser(selenium, browser_id)
    _is_marked_as_home(driver, item_name, item_type, panel)


@when(parsers.re('user of (?P<browser_id>.+?) sees that item named '
                 '"(?P<item_name>.+?)" in (?P<item_type>.+?)s list '
                 'in expanded "(?P<panel>.+?)" Onezone panel '
                 'is marked as home (?P=item_type)'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that item named '
                 '"(?P<item_name>.+?)" in (?P<item_type>.+?)s list '
                 'in expanded "(?P<panel>.+?)" Onezone panel '
                 'is marked as home (?P=item_type)'))
def wt_is_marked_as_home_item(selenium, browser_id, item_name,
                              item_type, panel):
    driver = select_browser(selenium, browser_id)
    _is_marked_as_home(driver, item_name, item_type, panel)


@when(parsers.re('user of (?P<browser_id>.+?) sees that submenu for '
                 '(?P<item_type>.+?) named "(?P<item_name>.+?)" '
                 'in (?P=item_type)s list in expanded "(?P<panel>.+?)" '
                 'Onezone panel has been expanded'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that submenu for '
                 '(?P<item_type>.+?) named "(?P<item_name>.+?)" '
                 'in (?P=item_type)s list in expanded "(?P<panel>.+?)" '
                 'Onezone panel has been expanded'))
def oz_wt_has_submenu_for_item_been_expanded(selenium, browser_id, item_type,
                                             item_name, panel):
    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_BACKEND).until(
        lambda d: get_submenu_for_item_from_oz_panel_list(d, item_name,
                                                          item_type, panel),
        message='waiting for submenu for "{}" in {} list to appear'
                ''.format(item_name, item_type)
    )
