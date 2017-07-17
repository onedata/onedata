"""Utils and fixtures to facilitate provider operations in op panel GUI."""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import (Label, NamedButton,
                                               Input, WebItem)


class UserAccountDetails(PageObject):
    username = Label('.field-generic-username')
    password = Label('.field-static-secretPassword')


class UserAccountsForm(PageObject):
    username = Label('.field-generic-username')
    current_password = Input('input.field-change-currentPassword')
    new_password = Input('input.field-change-newPassword')
    retype_new_password = Input('input.field-change-newPasswordRetype')
    confirm_password_change = NamedButton('button',
                                          text='Confirm password change')


class AccountManagementContentPage(PageObject):
    user_details = WebItem('.user-credentials-form', cls=UserAccountDetails)
    chpasswd_form = WebItem('.user-credentials-form', cls=UserAccountsForm)
    change_password = NamedButton('.btn-change-password',
                                  text='Change password')
    cancel_password_change = NamedButton('.btn-change-password',
                                         text='Cancel password change')
