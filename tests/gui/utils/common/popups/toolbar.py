"""Utils and fixtures to facilitate operations on user account menu popup.
"""

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import WebItemsSequence
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class ToolbarPopup(PageObject):
    options = WebItemsSequence('li a.clickable', cls=ButtonWithTextPageObject)

    def __str__(self):
        return 'User account popup'
