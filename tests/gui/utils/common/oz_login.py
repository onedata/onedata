"""Utils and fixtures to facilitate operations on oz login page"""

from ..core.web_elements import Input, NamedButton, Label, Button

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class OnezoneLoginPage(object):
    zone_name = Label('.zone-company-name')
    username_login = Button('.login-icon-box.username')
    username = Input('input[placeholder="Username"]')
    password = Input('input[placeholder="Password"]')
    sign_in = NamedButton('button', text='Sign in')
    err_msg = Label('.login-error-message')

    def __init__(self, driver):
        self.web_elem = self.driver = driver

    def __str__(self):
        return 'Onezone Login page'
