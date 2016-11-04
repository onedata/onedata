"""Steps for features of Onezone login page.
"""

__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from tests.gui.steps.onezone_logged_in_common import panel_to_css
from tests.utils.acceptance_utils import list_parser

from selenium.webdriver.support.ui import WebDriverWait as Wait

from pytest_bdd import given, when, then, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser


def _get_items_from_panel_list(driver, panel, item_type):
    panel = panel_to_css[panel.lower()]
    items = driver.find_elements_by_css_selector('{0} .{1}s-accordion-item, '
                                                 '{0} .{1}s-accordion-item '
                                                 '.{1}s-accordion-heading '
                                                 '.{1}-header'.format(panel,
                                                                      item_type))

    return {label.text: item for label, item in zip(items[1::2], items[::2])}


def _get_item_from_panel_list(driver, item_name, item_type, panel):
    items = _get_items_from_panel_list(driver, panel, item_type)
    item = items.get(item_name)
    if not item:
        raise ValueError('no {:s} named {:s} found'.format(item_type,
                                                           item_name))
    else:
        return item


def _not_in_panel_list(driver, panel, item_type, item_list):
    items = _get_items_from_panel_list(driver, panel, item_type)
    for item in list_parser(item_list):
        if item in items:
            return False
    return True


TOOL_TO_CSS = {'home outline icon': 'oneicon-home-outline',
               'home icon': 'oneicon-home'}


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
        lambda d: _not_in_panel_list(driver, panel, item_type, items),
        message='checking for absence of {items} on {list} list'
                ''.format(items=items, list=item_type)
    )


@when(parsers.re(r'user of (?P<browser_id>.*?) sees that (?P<items>.*?) '
                 r'(has|have) appeared on (?P<item_type>.*?)s list '
                 r'in expanded "(?P<panel>.+?)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.*?) sees that (?P<items>.*?) '
                 r'(has|have) appeared on (?P<item_type>.*?)s list '
                 r'in expanded "(?P<panel>.+?)" Onezone panel'))
@when(parsers.re(r'user of (?P<browser_id>.*?) sees (?P<items>.*?) '
                 r'on (?P<item_type>.*?)s list '
                 r'in expanded "(?P<panel>.+?)" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.*?) sees (?P<items>.*?) '
                 r'on (?P<item_type>.*?)s list '
                 r'in expanded "(?P<panel>.+?)" Onezone panel'))
def is_present_in_panel_list(selenium, browser_id, items,
                             item_type, panel, tmp_memory):
    driver = select_browser(selenium, browser_id)
    items = tmp_memory[browser_id]['gen_str'] if items == 'new item' else items
    Wait(driver, WAIT_BACKEND).until_not(
        lambda d: _not_in_panel_list(driver, panel, item_type, items),
        message='checking for presence of {items} on {list} list'
                ''.format(items=items, list=item_type)
    )


@when(parsers.parse('user of {browser_id} clicks on {tool_name} in item row '
                    'for item named "{item_name}" in {item_type} list in '
                    'in expanded "{panel}" Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on {tool_type} in item row '
                    'for item named "{item_name}" in {item_type} list in '
                    'expanded "{panel}" Onezone panel'))
def click_on_tool_in_item_row(selenium, browser_id, tool_name, item_name,
                              item_type, panel):
    driver = select_browser(selenium, browser_id)
    item = _get_item_from_panel_list(driver, item_name, item_type, panel)
    tool = '.{}'.format(TOOL_TO_CSS[tool_name])
    item.find_element_by_css_selector(tool).click()


def _is_marked_as_home(driver, item_name, item_type, panel):
    item = _get_item_from_panel_list(driver, item_name, item_type, panel)
    assert item.find_element_by_css_selector('.oneicon-home')
    assert item.find_element_by_css_selector('.oneicon-{}-home'.format(item_type))


@given(parsers.re('user of (?P<browser_id>.+?) seen that item named '
                  '"(?P<item_name>.+?)" in (?P<item_type>.+?)s list in expanded '
                  '"(?P<panel>.+?)" Onezone panel '
                  'was marked as home (?P=item_type)'))
def g_is_marked_as_home_item(selenium, browser_id, item_name, item_type, panel):
    driver = select_browser(selenium, browser_id)
    _is_marked_as_home(driver, item_name, item_type, panel)


@when(parsers.re('user of (?P<browser_id>.+?) sees that item named '
                 '"(?P<item_name>.+?)" in (?P<item_type>.+?)s list in expanded '
                 '"(?P<panel>.+?)" Onezone panel '
                 'is marked as home (?P=item_type)'))
@then(parsers.re('user of (?P<browser_id>.+?) sees that item named '
                 '(?P<item_name>.+?) in (?P<item_type>.+?)s list in expanded '
                 '"(?P<panel>.+?)" Onezone panel '
                 'is marked as home (?P=item_type)'))
def wt_is_marked_as_home_item(selenium, browser_id, item_name, item_type, panel):
    driver = select_browser(selenium, browser_id)
    _is_marked_as_home(driver, item_name, item_type, panel)


def _click_on_list_item(driver, item_name, item_type, panel):
    _get_item_from_panel_list(driver, item_name, item_type, panel).clik()


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
