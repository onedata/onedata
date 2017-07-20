"""This module contains gherkin steps to run acceptance tests featuring
account management in onezone web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


from pytest_bdd import when, then, parsers

from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed, transform


@when(parsers.parse('user of {browser_id} clicks on user account '
                    'button in main menu'))
@then(parsers.parse('user of {browser_id} clicks on user account '
                    'button in main menu'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_click_on_user_account_btn_panel(selenium, browser_id, onepage):
    onepage(selenium[browser_id]).account()


@when(parsers.parse('user of {browser_id} clicks on {btn} button '
                    'in user account popover'))
@then(parsers.parse('user of {browser_id} clicks on {btn} button '
                    'in user account popover'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_click_option_in_user_account_popover(selenium, browser_id, btn, popups):
    popups(selenium[browser_id]).user_account_menu.options[btn].click()


@when(parsers.parse('user of {browser_id} types password for {user} '
                    'in Current password in change password form '
                    'in account management page'))
@then(parsers.parse('user of {browser_id} types password for {user} '
                    'in Current password in change password form '
                    'in account management page'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_password_of_user_to_curr_passwd(selenium, browser_id, user,
                                            users, onepage):
    form = (onepage(selenium[browser_id])
            .content
            .account_management
            .chpasswd_form)
    form.current_password = users[user].password


@when(parsers.re('user of (?P<browser_id>.*?) types "(?P<text>.*?)" to '
                 '(?P<in_box>New|Retype new|Current) password in change '
                 'password form in account management page'))
@then(parsers.re('user of (?P<browser_id>.*?) types "(?P<text>.*?)" to '
                 '(?P<in_box>New|Retype new|Current) password in change '
                 'password form in account management page'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_text_to_in_box_in_chpasswd_form(selenium, browser_id, in_box,
                                            text, onepage):
    form = (onepage(selenium[browser_id])
            .content
            .account_management
            .chpasswd_form)
    setattr(form, transform(in_box + ' password'), text)


@when(parsers.re('user of (?P<browser_id>.*?) clicks on Confirm password change '
                 'button in change password form in account management page'))
@then(parsers.re('user of (?P<browser_id>.*?) clicks on Confirm password change '
                 'button in change password form in account management page'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_click_confirm_btn_in_chpasswd_form(selenium, browser_id, onepage):
    form = (onepage(selenium[browser_id])
            .content
            .account_management
            .chpasswd_form)
    form.confirm_password_change()


@when(parsers.parse('user of {browser_id} clicks on {btn} '
                    'button in account management page'))
@then(parsers.parse('user of {browser_id} clicks on {btn} '
                    'button in account management page'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_click_on_btn_in_account_management(selenium, browser_id,
                                          btn, onepage):
    getattr(onepage(selenium[browser_id]).content.account_management,
            transform(btn)).click()
