"""Steps for features of Oneprovider's groups.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from selenium.webdriver.support.ui import WebDriverWait as Wait
from pytest_bdd import given, when, then, parsers

from common import check_if_element_is_active



# sprawdzanie file dist czy jest ten prov na ktorego sie weszlo



# @when('user types new group name on keyboard')
# def clear_and_type_string_into_active_element(selenium, random_name):
#     selenium.switch_to.active_element.clear()
#     selenium.switch_to.active_element.send_keys(random_name)
#
#
# @then('user should see popup with information about name change')
# def check_confirmation_after_rename(selenium, name, random_name):
#     from common import notify_visible_with_text
#     notify_visible_with_text(selenium, "info", '.*' + name + '.*renamed.*' + random_name + '.*')
#
#
# @then(parsers.parse('user should see, that the new name replaced old one on the list'))
# def renamed_group(selenium, name, random_name):
#
#     def header_with_text_presence(s):
#         headers = s.find_elements_by_css_selector('.groups-list .secondary-sidebar-item .item-label .truncate')
#         return all(h.text != name for h in headers) and any(h.text == random_name for h in headers)
#
#     Wait(selenium, WAIT_BACKEND).until(header_with_text_presence)
#
#     # restore previous name
#     click_on_settings_button_in_group_panel(selenium, random_name)
#     click_on_elem(selenium, "RENAME")
#     wait_for_rename_input_box(selenium)
#     clear_and_type_string_into_active_element(selenium, name)
#     selenium.switch_to.active_element.send_keys(Keys.RETURN)
