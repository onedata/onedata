"""Utils and fixtures to facilitate provider operations in op panel GUI."""

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import (Label, NamedButton,
                                               Input, WebItem)

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class UserAccountDetails(PageObject):
    username = Label('.field-username')
    password = Label('.field-secretPassword')

    def __str__(self):
        return 'Account details in {}'.format(self.parent)


class UserAccountsForm(PageObject):
    username = Label('.field-username')
    current_password = Input('input.field-currentPassword')
    new_password = Input('input.field-newPassword')
    retype_new_password = Input('input.field-newPasswordRetype')
    confirm_password_change = NamedButton('button',
                                          text='Confirm password change')

    def __str__(self):
        return 'User password change form in {}'.format(self.parent)


class AccountManagementContentPage(PageObject):
    user_details = WebItem('.user-credentials-form', cls=UserAccountDetails)
    chpasswd_form = WebItem('.user-credentials-form', cls=UserAccountsForm)
    change_password = NamedButton('.btn-change-password',
                                  text='Change password')
    cancel_password_change = NamedButton('.btn-change-password',
                                         text='Cancel password change')

    def __str__(self):
        return 'Account management page in {}'.format(self.parent)
