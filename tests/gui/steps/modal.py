"""Steps used for modal handling in various GUI testing scenarios
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND
from selenium.webdriver.support.ui import WebDriverWait as Wait
from pytest_bdd import parsers, when, then
from pytest_selenium_multi.pytest_selenium_multi import select_browser


def _find_modal(driver, modal_name):
    def _get_modal():
        for name, modal in zip(modals[1::2], modals[::2]):
            if name.text.lower() == modal_name:
                return modal

    modal_name = modal_name.lower()
    modals = driver.find_elements_by_css_selector('.modal.in, '
                                                  '.modal.in .modal-title')
    return Wait(driver, WAIT_FRONTEND).until(
        lambda _: _get_modal(),
        message='waiting for {:s} modal to appear'.format(modal_name)
    )


@when(parsers.parse('user of {browser_id} sees that '
                    '"{modal_name}" modal has appeared'))
@then(parsers.parse('user of {browser_id} sees that '
                    '"{modal_name}" modal has appeared'))
def wait_for_modal_to_appear(selenium, browser_id, modal_name, tmp_memory):
    driver = select_browser(selenium, browser_id)
    modal = _find_modal(driver, modal_name)
    tmp_memory[browser_id]['window']['modal'] = modal


@when(parsers.parse('user of {browser_id} sees that '
                    'modal has disappeared'))
@then(parsers.parse('user of {browser_id} sees that '
                    'modal has disappeared'))
def wait_for_modal_to_disappear(selenium, browser_id, tmp_memory):
    driver = select_browser(selenium, browser_id)
    modal = tmp_memory[browser_id]['window']['modal']
    Wait(driver, WAIT_FRONTEND).until_not(
        lambda _: modal.is_displayed(),
        message='waiting for modal to disappear'
    )
    tmp_memory[browser_id]['window']['modal'] = None


@when(parsers.parse('user of {browser_id} clicks "{button_name}" '
                    'confirmation button in displayed modal'))
@then(parsers.parse('user of {browser_id} clicks "{button_name}" '
                    'confirmation button in displayed modal'))
def op_click_confirmation_button_in_displayed_modal(browser_id,
                                                    button_name,
                                                    tmp_memory):
    button_name = button_name.lower()
    modal = tmp_memory[browser_id]['window']['modal']
    buttons = modal.find_elements_by_css_selector('button')
    for button in buttons:
        if button.text.lower() == button_name:
            button.click()
            break


@when(parsers.parse('user of {browser_id} sees '
                    'non-empty token in active modal'))
@then(parsers.parse('user of {browser_id} sees '
                    'non-empty token in active modal'))
def get_non_empty_token_from_modal(selenium, browser_id, tmp_memory):
    driver = select_browser(selenium, browser_id)
    modal = tmp_memory[browser_id]['window']['modal']
    token_box = modal.find_element_by_css_selector('input[readonly]')
    token = Wait(driver, WAIT_BACKEND).until(
        lambda _: token_box.get_attribute('value'),
        message='waiting for token to appear'
    )
    tmp_memory[browser_id]['token'] = token


@when(parsers.re(r'user of (?P<browser_id>.*?) clicks on '
                 r'((?P<input_type>.*?) )?input box in active modal'))
@then(parsers.re(r'user of (?P<browser_id>.*?) clicks on '
                 r'((?P<input_type>.*?) )?input box in active modal'))
def get_non_empty_token_from_modal(browser_id, input_type, tmp_memory):
    modal = tmp_memory[browser_id]['window']['modal']
    in_box = modal.find_element_by_css_selector('input.{}'.format(input_type)
                                                if input_type
                                                else 'input')
    in_box.click()


@when(parsers.parse('user of {browser_id} clicks on '
                    'copy button in active modal'))
@then(parsers.parse('user of {browser_id} clicks on '
                    'copy button in active modal'))
def click_on_copy_btn_in_modal(browser_id, tmp_memory):
    modal = tmp_memory[browser_id]['window']['modal']
    copy_btn = modal.find_element_by_css_selector('button.copy-btn')
    copy_btn.click()
