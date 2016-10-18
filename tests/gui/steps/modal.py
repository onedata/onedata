"""Steps used for modal handling in various GUI testing scenarios
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.conftest import WAIT_FRONTEND, WAIT_BACKEND

from selenium.webdriver.support.ui import WebDriverWait as Wait
from selenium.webdriver.support.expected_conditions import staleness_of
from selenium.webdriver.common.keys import Keys

from pytest_bdd import parsers, when, then
from pytest_selenium_multi.pytest_selenium_multi import select_browser


in_type_to_id = {'username': 'login-form-username-input',
                 'password': 'login-form-password-input'}


def _find_modal(driver, modal_name):
    def _find():
        for name, modal in zip(modals[1::2], modals[::2]):
            if name.text.lower() == modal_name:
                return modal

    modal_name = modal_name.lower()
    modals = driver.find_elements_by_css_selector('.modal.in, '
                                                  '.modal.in .modal-title')
    return Wait(driver, WAIT_FRONTEND).until(
        lambda _: _find(),
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
                    'the modal has disappeared'))
@then(parsers.parse('user of {browser_id} sees that '
                    'the modal has disappeared'))
def wait_for_modal_to_disappear(selenium, browser_id, tmp_memory):
    driver = select_browser(selenium, browser_id)
    modal = tmp_memory[browser_id]['window']['modal']
    Wait(driver, WAIT_FRONTEND).until_not(
        lambda _: not staleness_of(modal) or modal.is_displayed(),
        message='waiting for modal to disappear'
    )
    tmp_memory[browser_id]['window']['modal'] = None


@when(parsers.parse('user of {browser_id} clicks "{button_name}" '
                    'confirmation button in displayed modal'))
@then(parsers.parse('user of {browser_id} clicks "{button_name}" '
                    'confirmation button in displayed modal'))
def click_on_confirmation_btn_in_modal(selenium, browser_id, button_name,
                                       tmp_memory):
    driver = select_browser(selenium, browser_id)
    button_name = button_name.lower()
    modal = tmp_memory[browser_id]['window']['modal']
    buttons = modal.find_elements_by_css_selector('button')
    for btn in buttons:
        if btn.text.lower() == button_name:
            Wait(driver, WAIT_FRONTEND).until(
                lambda _: btn.is_displayed() and btn.is_enabled(),
                message='waiting for button {} to be enabled'
                        ''.format(button_name)
            )
            btn.click()
            break
    else:
        raise ValueError('no button named {} found'.format(button_name))


@when(parsers.parse('user of {browser_id} sees '
                    'non-empty token in active modal'))
@then(parsers.parse('user of {browser_id} sees '
                    'non-empty token in active modal'))
def get_token_from_modal(selenium, browser_id, tmp_memory):
    driver = select_browser(selenium, browser_id)
    modal = tmp_memory[browser_id]['window']['modal']
    token_box = modal.find_element_by_css_selector('input[readonly]')
    token = Wait(driver, WAIT_BACKEND).until(
        lambda _: token_box.get_attribute('value'),
        message='waiting for token to appear'
    )
    tmp_memory[browser_id]['token'] = token


@when(parsers.re(r'user of (?P<browser_id>.*?) clicks on '
                 r'((?P<in_type>.*?) )?input box in active modal'))
@then(parsers.re(r'user of (?P<browser_id>.*?) clicks on '
                 r'((?P<in_type>.*?) )?input box in active modal'))
def activate_input_box_in_modal(browser_id, in_type, tmp_memory):
    modal = tmp_memory[browser_id]['window']['modal']
    css_path = 'input#{}'.format(in_type_to_id[in_type]) if in_type else 'input'
    in_box = modal.find_element_by_css_selector(css_path)
    # send NULL to activates input box
    in_box.send_keys(Keys.NULL)


@when(parsers.parse('user of {browser_id} clicks on '
                    'copy button in active modal'))
@then(parsers.parse('user of {browser_id} clicks on '
                    'copy button in active modal'))
def click_on_copy_btn_in_modal(browser_id, tmp_memory):
    modal = tmp_memory[browser_id]['window']['modal']
    copy_btn = modal.find_element_by_css_selector('button.copy-btn')
    copy_btn.click()
