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


@when('user should see that name input box is active')
def wait_for_input_box(selenium):
    is_active = check_if_element_is_active('#create-group-modal input')
    Wait(selenium, WAIT_FRONTEND).until(is_active)


@when('user should see that rename input box is active')
def wait_for_rename_input_box(selenium):
    is_active = check_if_element_is_active('#rename-group-modal input')
    Wait(selenium, WAIT_FRONTEND).until(is_active)


@when('user should see that join space input box is active')
def wait_for_join_space_input_box(selenium):
    is_active = check_if_element_is_active('#join-group-to-space-modal input')
    Wait(selenium, WAIT_FRONTEND).until(is_active)


@when('user should see that join as subgroup input box is active')
def wait_for_join_group_input_box(selenium):
    is_active = check_if_element_is_active('#join-group-to-group-modal input')
    Wait(selenium, WAIT_FRONTEND).until(is_active)


@when('user should see that join group input box is active')
def wait_for_join_group_input_box(selenium):
    is_active = check_if_element_is_active('#join-group-modal input')
    Wait(selenium, WAIT_FRONTEND).until(is_active)


@when('user should see that request input box is active')
@when('user should see that invite input box is active')
def wait_for_input_box(selenium):
    is_active = check_if_element_is_active('#token-undefined-modal input')
    Wait(selenium, WAIT_FRONTEND).until(is_active)


@then('user can copy visible invite-user token')
def copy_invite_user_token(selenium):
    assert Wait(selenium, WAIT_BACKEND).until(
        lambda s: s.find_element_by_css_selector(
            '.input-with-button '
            '#invite-form-token-userJoinGroup '
            'input#invite-form-token-userJoinGroup-field')
    ).get_attribute('value')


@then('user can copy visible invite-group token')
def copy_invite_user_token(selenium):
    assert Wait(selenium, WAIT_BACKEND).until(
        lambda s: s.find_element_by_css_selector(
            '.input-with-button '
            '#invite-form-token-groupJoinGroup '
            'input#invite-form-token-groupJoinGroup-field')
    ).get_attribute('value')


@then('user can copy visible request token')
def copy_invite_user_token(selenium):
    assert Wait(selenium, WAIT_BACKEND).until(
        lambda s: s.find_element_by_css_selector(
            '.input-with-button '
            '#invite-form-token-requestSpaceCreation '
            'input#invite-form-token-requestSpaceCreation-field')
    ).get_attribute('value')
