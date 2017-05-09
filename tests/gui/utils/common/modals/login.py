"""Utils and fixtures to facilitate operations on Login modal.
"""

from tests.gui.utils.core.web_elements import Input
from .modal import Modal

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class LoginFormModal(Modal):
    username = Input('input#login-form-username-input')
    password = Input('input#login-form-password-input')

    def __str__(self):
        return 'Login modal'
