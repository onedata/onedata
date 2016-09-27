"""Steps used for modal handling in various GUI testing scenarios
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from pytest_bdd import when, then, parsers
from selenium.webdriver.common.action_chains import ActionChains
from pytest_selenium_multi.pytest_selenium_multi import select_browser
from tests.gui.conftest import WAIT_FRONTEND
from selenium.webdriver.support.ui import WebDriverWait as Wait

from tests.utils.acceptance_utils import list_parser


tool_type_to_icon = {'share': 'oneicon-share'}


type_to_icon = {'shared-directory': 'oneicon-folder-share',
                'directory': 'oneicon-folder',
                'file': 'oneicon-file'}


def _get_items_from_file_list(driver):
    files = driver.find_elements_by_css_selector('table.files-table '
                                                 'tr,'
                                                 'table.files-table '
                                                 'td.file-list-col-file '
                                                 '.file-icon .oneicon, '
                                                 'table.files-table '
                                                 'td.file-list-col-file '
                                                 '.file-label, '
                                                 'table.files-table '
                                                 'td.file-list-col-file '
                                                 '.file-row-tools, '
                                                 'table.files-table '
                                                 'td.file-list-col-size,'
                                                 'table.files-table '
                                                 'td.file-list-col-modification')
    return {label.text: (row, label, icon, tools, size, modification)
            for row, label, icon, tools, size, modification
            in zip(files[::6], files[2::6], files[1::6],
                   files[3::6], files[4::6], files[5::6])}


def _not_in_file_list(driver, items, items_type, file_list=None):
    file_list = file_list if file_list else _get_items_from_file_list(driver)
    icon = type_to_icon[items_type]
    for item_name in list_parser(items):
        item = file_list.get(item_name)
        if item and icon in item[2].get_attribute('class'):
                return False
    return True


def _double_click_on_item(driver, item_name, item_type, items=None):
    items = items if items else _get_items_from_file_list(driver)
    item = items.get(item_name)
    if item:
        assert type_to_icon[item_type] in item[2].get_attribute('class')
        ActionChains(driver).double_click(item[1]).perform()
    else:
        raise ValueError('no {} named {} found'. format(item_type, item_name))


def _select_items_from_file_list(driver, item_list, all_items=None):
    all_items = all_items if all_items else _get_items_from_file_list(driver)
    for item_name in list_parser(item_list):
        item = all_items.get(item_name)
        if item and 'active' not in item[0].get_attribute('class'):
                item[1].click()


def _click_on_file_tool_icon(driver, item_name, item_type,
                             tool_type, items=None):
    items = items if items else _get_items_from_file_list(driver)
    item = items.get(item_name)
    if item and type_to_icon[item_type] in item[2].get_attribute('class'):
        css_path = '.{}'.format(tool_type_to_icon[tool_type])
        item[3].find_element_by_css_selector(css_path).click()
    else:
        raise ValueError('no {} named {} found'.format(item_type, item_name))


@when(parsers.re(r'user of (?P<browser_id>.*?) sees that (?P<item_list>.*?) '
                 r'(?P<item_type>.*?)s? (has|have) disappeared from file list'))
@then(parsers.re(r'user of (?P<browser_id>.*?) sees that (?P<item_list>.*?) '
                 r'(?P<item_type>.*?)s? (has|have) disappeared from file list'))
@when(parsers.re(r'user of (?P<browser_id>.*?) does not see (?P<item_list>.*?) '
                 r'as (?P<item_type>.*?)s? in file list'))
@then(parsers.re(r'user of (?P<browser_id>.*?) does not see (?P<item_list>.*?) '
                 r'as (?P<item_type>.*?)s? in file list'))
def is_not_present_in_file_list(selenium, browser_id, item_list, item_type):
    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until(
        lambda _: _not_in_file_list(driver, item_list, item_type),
        message='waiting for {:s} item/items '
                'to disappear from file list'.format(item_list)
    )


@when(parsers.re(r'user of (?P<browser_id>.*?) sees that (?P<item_list>.*?) '
                 r'(?P<item_type>.*?)s? (has|have) appeared in file list'))
@then(parsers.re(r'user of (?P<browser_id>.*?) sees that (?P<item_list>.*?) '
                 r'(?P<item_type>.*?)s? (has|have) appeared in file list'))
@when(parsers.re(r'user of (?P<browser_id>.*?) sees (?P<item_list>.*?) '
                 r'as (?P<item_type>.*?)s? in file list'))
@then(parsers.re(r'user of (?P<browser_id>.*?) sees (?P<item_list>.*?) '
                 r'as (?P<item_type>.*?)s? in file list'))
def is_present_in_file_list(selenium, browser_id, item_list, item_type):
    driver = select_browser(selenium, browser_id)
    Wait(driver, WAIT_FRONTEND).until_not(
        lambda _: _not_in_file_list(driver, item_list, item_type),
        message='waiting for {:s} item/items '
                'to appear in file list'.format(item_list)
    )


@then(parsers.parse('user of {browser_id} double clicks '
                    'on {item_type} {item_name} from files list'))
@when(parsers.parse('user of {browser_id} double clicks '
                    'on {item_type} {item_name} from files list'))
def double_click_on_item(selenium, browser_id, item_name,
                         item_type):
    driver = select_browser(selenium, browser_id)
    _double_click_on_item(driver, item_name, item_type)


@when(parsers.parse('user of {browser_id} selects {item_list} '
                    'from files list'))
@then(parsers.parse('user of {browser_id} selects {item_list} '
                    'from files list'))
def select_files_from_file_list(selenium, browser_id, item_list):
    driver = select_browser(selenium, browser_id)
    _select_items_from_file_list(driver, item_list)


@when(parsers.parse("user of {browser_id} clicks on {tool_type} "
                    "icon in tools column for {file_name} {file_type}"))
@then(parsers.parse("user of {browser_id} clicks on {tool_type} "
                    "icon in tools column for {file_name} {file_type}"))
def click_on_file_icon_tool(selenium, browser_id, tool_type,
                            file_name, file_type):
    driver = select_browser(selenium, browser_id)
    _click_on_file_tool_icon(driver, file_name, file_type, tool_type)
