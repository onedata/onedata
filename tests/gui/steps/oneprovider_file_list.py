"""Steps used for file list handling in various GUI testing scenarios
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from pytest_bdd import when, then, parsers

from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait as Wait

from pytest_selenium_multi.pytest_selenium_multi import select_browser

from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.generic import parse_seq


tool_type_to_icon = {'share': 'oneicon-share'}


type_to_icon = {'shared directory': 'oneicon-folder-share',
                'directory': 'oneicon-folder',
                'file': 'oneicon-file'}


def _pack_content_into_rows(items):
    items_num = len(items)
    index = 0
    while index < items_num:
        row, label, icon, tools, size, mod = (items[index], items[index+2],
                                              items[index+1], items[index+3],
                                              items[index+4], items[index+5])
        meta_index = index + 6
        while meta_index < items_num and items[meta_index].tag_name == 'tr':
            meta_index += 1

        if meta_index - index > 7:
            meta = items[index + 6]
            index = meta_index - 1 if meta_index < items_num else items_num
        else:
            meta = None
            index += 6

        yield (row, label, icon, tools, size, mod, meta)


def _get_items_from_file_list(driver):
    items = driver.find_elements_by_css_selector('table.files-table '
                                                 'tr.file-row, '
                                                 'table.files-table '
                                                 'tr.file-row '
                                                 'td.file-list-col-file '
                                                 '.file-icon .oneicon, '
                                                 'table.files-table '
                                                 'tr.file-row '
                                                 'td.file-list-col-file '
                                                 '.file-label, '
                                                 'table.files-table '
                                                 'tr.file-row '
                                                 'td.file-list-col-file '
                                                 '.file-row-tools, '
                                                 'table.files-table '
                                                 'tr.file-row '
                                                 'td.file-list-col-size, '
                                                 'table.files-table '
                                                 'tr.file-row '
                                                 'td.file-list-col-modification')

    return {label.text: (row, label, icon, tools, size, modification, meta)
            for row, label, icon, tools, size, modification, meta
            in _pack_content_into_rows(items)}


def _not_in_file_list(driver, items, items_type, file_list=None):
    file_list = file_list if file_list else _get_items_from_file_list(driver)
    icon = type_to_icon[items_type]
    for item_name in parse_seq(items):
        item = file_list.get(item_name)
        if item and icon in item[2].get_attribute('class'):
                return False
    return True


def _double_click_on_item(driver, item_name, item_type, items=None):
    items = items if items else _get_items_from_file_list(driver)
    item = items.get(item_name)
    if item and type_to_icon[item_type] in item[2].get_attribute('class'):
        ActionChains(driver).double_click(item[1]).perform()
    else:
        raise ValueError('no {} named {} found'. format(item_type, item_name))


def _is_file_selected(file_item):
    return file_item and 'active' in file_item[0].get_attribute('class')


def _is_not_file_selected(file_item):
    return file_item and 'active' not in file_item[0].get_attribute('class')


def _select_items_from_file_list_upon_cond(driver, item_list, keys,
                                           cond=_is_file_selected,
                                           all_items=None):
    all_items = all_items if all_items else _get_items_from_file_list(driver)
    actions = ActionChains(driver)
    for key in keys:
        actions.key_down(key)

    for item_name in parse_seq(item_list):
        item = all_items.get(item_name)
        if cond(item):
                actions.click(item[0])

    for key in keys:
        actions.key_up(key)
    actions.perform()


def _click_on_tool_icon_for_file(driver, item_name, item_type,
                                 tool_type, items=None):
    items = items if items else _get_items_from_file_list(driver)
    item = items.get(item_name)
    if item and type_to_icon[item_type] in item[2].get_attribute('class'):
        css_path = '.{}'.format(tool_type_to_icon[tool_type])
        item[3].find_element_by_css_selector(css_path).click()
    else:
        raise ValueError('no {} named {} found'.format(item_type, item_name))


@when(parsers.re(r'user of (?P<browser_id>.*?) sees that (?P<item_type>.*?)s? '
                 r'named (?P<item_list>.*?) (has|have) disappeared from files list'))
@then(parsers.re(r'user of (?P<browser_id>.*?) sees that (?P<item_type>.*?)s? '
                 r'named (?P<item_list>.*?) (has|have) disappeared from files list'))
@when(parsers.re(r'user of (?P<browser_id>.*?) does not see any '
                 r'(?P<item_type>.*?)s? named (?P<item_list>.*?) on files list'))
@then(parsers.re(r'user of (?P<browser_id>.*?) does not see any '
                 r'(?P<item_type>.*?)s? named (?P<item_list>.*?) on files list'))
def is_not_present_in_file_list(selenium, browser_id, item_list, item_type):
    driver = select_browser(selenium, browser_id)
    item_type = item_type.replace('directorie', 'directory')
    Wait(driver, WAIT_FRONTEND).until(
        lambda _: _not_in_file_list(driver, item_list, item_type),
        message='waiting for {:s} item/items '
                'to disappear from file list'.format(item_list)
    )


@when(parsers.re(r'user of (?P<browser_id>.*?) sees (?P<item_type>.*?)s? '
                 r'named (?P<item_list>.*?) on files list'))
@then(parsers.re(r'user of (?P<browser_id>.*?) sees (?P<item_type>.*?)s? '
                 r'named (?P<item_list>.*?) on files list'))
@when(parsers.re(r'user of (?P<browser_id>.*?) sees that (?P<item_type>.*?)s? '
                 r'named (?P<item_list>.*?) (has|have) appeared on files list'))
@then(parsers.re(r'user of (?P<browser_id>.*?) sees that (?P<item_type>.*?)s? '
                 r'named (?P<item_list>.*?) (has|have) appeared on files list'))
def is_present_in_file_list(selenium, browser_id, item_list, item_type):
    driver = select_browser(selenium, browser_id)
    item_type = item_type.replace('directorie', 'directory')
    Wait(driver, WAIT_FRONTEND).until_not(
        lambda _: _not_in_file_list(driver, item_list, item_type),
        message='waiting for {:s} item/items '
                'to appear in file list'.format(item_list)
    )


@then(parsers.parse('user of {browser_id} double clicks '
                    'on {item_type} named "{item_name}" from files list'))
@when(parsers.parse('user of {browser_id} double clicks '
                    'on {item_type} named "{item_name}" from files list'))
def double_click_on_file_item(selenium, browser_id, item_name,
                              item_type):
    driver = select_browser(selenium, browser_id)
    _double_click_on_item(driver, item_name, item_type)


@when(parsers.parse('user of {browser_id} clicks on {item_type} '
                    'named "{item_name}" from files list'))
@then(parsers.parse('user of {browser_id} clicks on {item_type} '
                    'named "{item_name}" from files list'))
def click_on_file_item(selenium, browser_id, item_name,
                       item_type):
    driver = select_browser(selenium, browser_id)
    all_items = _get_items_from_file_list(driver)
    item = all_items.get(item_name)
    if item and type_to_icon[item_type] in item[2].get_attribute('class'):
        item[0].click()
    else:
        raise ValueError('no {} named {} found'.format(item_type, item_name))


@when(parsers.parse('user of {browser_id} selects {item_list} '
                    'from files list'))
@then(parsers.parse('user of {browser_id} selects {item_list} '
                    'from files list'))
def select_files_from_file_list(selenium, browser_id, item_list):
    driver = select_browser(selenium, browser_id)
    _select_items_from_file_list_upon_cond(driver, item_list,
                                           keys=(Keys.LEFT_CONTROL, ),
                                           cond=_is_not_file_selected)


@when(parsers.parse('user of {browser_id} deselects {item_list} '
                    'from files list'))
@then(parsers.parse('user of {browser_id} deselects {item_list} '
                    'from files list'))
def deselect_files_from_file_list(selenium, browser_id, item_list):
    driver = select_browser(selenium, browser_id)
    _select_items_from_file_list_upon_cond(driver, item_list,
                                           keys=(Keys.LEFT_CONTROL, ),
                                           cond=_is_file_selected)


@when(parsers.parse('user of {browser_id} deselects all '
                    'selected items from files list'))
@then(parsers.parse('user of {browser_id} deselects all '
                    'selected items from files list'))
def deselect_all_items_from_file_list(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    all_items = _get_items_from_file_list(driver)
    items_list = ', '.join(item for item in all_items.iterkeys())
    _select_items_from_file_list_upon_cond(driver, items_list,
                                           keys=(Keys.LEFT_CONTROL, ),
                                           cond=_is_file_selected)


@when(parsers.parse('user of {browser_id} clicks on {tool_type} '
                    'icon in file row for {file_type} named "{file_name}" '
                    'in file browser'))
@then(parsers.parse('user of {browser_id} clicks on {tool_type} '
                    'icon in file row for {file_type} named "{file_name}" '
                    'in file browser'))
def click_on_file_icon_tool(selenium, browser_id, tool_type,
                            file_name, file_type):
    driver = select_browser(selenium, browser_id)
    _click_on_tool_icon_for_file(driver, file_name, file_type, tool_type)


@when(parsers.parse('user of {browser_id} sees that {num:d} files '
                    'are displayed in file browser'))
@then(parsers.parse('user of {browser_id} sees that {num:d} files '
                    'are displayed in file browser'))
def check_how_many_files_are_displayed_in_file_browser(selenium, browser_id,
                                                       num):
    driver = select_browser(selenium, browser_id)
    items_num = len(_get_items_from_file_list(driver))
    assert items_num == num, '{:d} == {:d}'.format(items_num, num)


@when(parsers.parse('user of {browser_id} scrolls to the bottom '
                    'of file list in file browser'))
@then(parsers.parse('user of {browser_id} scrolls to the bottom '
                    'of file list in file browser'))
def scroll_to_the_bottom_of_file_browser(selenium, browser_id):
    driver = select_browser(selenium, browser_id)
    bottom = driver.find_element_by_css_selector('table.files-table '
                                                 'tr.file-row-load-more')
    driver.execute_script('arguments[0].scrollIntoView();', bottom)
