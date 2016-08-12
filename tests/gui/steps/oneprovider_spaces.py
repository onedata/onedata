from pytest_bdd import given, parsers, when, then
from tests.gui.steps.common import find_element
from selenium.webdriver.support.ui import WebDriverWait as Wait
from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
import selenium


@when(parsers.parse('user clicks space named "{space_name}" from spaces list'))
def click_space_name(selenium, space_name):
    space_to_click = find_element(selenium, 'ul.spaces-list .secondary-sidebar-item', space_name)
    space_to_click.click()


@then(parsers.parse('user should see home space icon next to "{space_name}"'))
def check_if_home_space_icon_next_to_spaces(selenium, space_name):

    def _find_home_space_icon(s):
        spaces = s.find_elements_by_css_selector('.ember-view ul.spaces-list .secondary-sidebar-item')
        for elem in spaces:
            if elem.find_element_by_css_selector('span.oneicon-space-home'):
                return elem
        return None

    assert _find_home_space_icon(selenium).text == space_name


@then(parsers.parse('user should see submenu for space named "{space_name}"'))
def check_if_displayed_space_menu(selenium, space_name):
    Wait(selenium, WAIT_FRONTEND).\
        until(lambda s: find_element(selenium,
                                     'li.active .secondary-sidebar-item .truncate',
                                     space_name) is not None)


#TODO
@then('user should not see input box for new name')
def wait_to_hide_input_box(selenium):

    def _is_input_box_hidden(s):
        elems = s.find_elements_by_css_selector('#rename-space-backdrop')
        if len(elems) == 0:
            return True
        return False

    Wait(selenium, WAIT_BACKEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR, '#rename-space-backdrop')))
    Wait(selenium, WAIT_FRONTEND).until(_is_input_box_hidden)


@when('user clicks "YES" button in popup window asking if he is sure')
def click_yes_button(selenium):
    yes_button = Wait(selenium, WAIT_FRONTEND).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, '#leave-space-modal form.ember-view button.btn-primary'))
    )
    yes_button.click()


@then('user should not see popup window')
def wait_to_hide_input_box(selenium):

    def _is_input_box_hidden(s):
        elems = s.find_elements_by_css_selector('#leave-space-backdrop')
        if len(elems) == 0:
            return True
        return False

    Wait(selenium, WAIT_BACKEND).until(
        EC.invisibility_of_element_located((By.CSS_SELECTOR, '#leave-space-backdrop')))
    Wait(selenium, WAIT_FRONTEND).until(_is_input_box_hidden)


