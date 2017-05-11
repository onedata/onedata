"""Utils for operations on modals in GUI tests
"""

from tests.gui.utils.core.web_elements import WebItem
from .deregister_provider import DeregisterProvider
from .user_account_menu import UserAccountPopup
from .revoke_space_support import RevokeSpaceSupport

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Popups(object):
    deregister_provider = WebItem('.popover-deregister-provider',
                                  cls=DeregisterProvider)
    revoke_space_support = WebItem('.popover-revoke-space',
                                   cls=RevokeSpaceSupport)
    user_account_menu = WebItem('.webui-popover-content .user-account-menu',
                                cls=UserAccountPopup)

    def __init__(self, driver):
        self.driver = self.web_elem = driver

    def __str__(self):
        return 'popups'
