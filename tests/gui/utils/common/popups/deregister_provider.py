"""Utils and fixtures to facilitate operations on deregister provider popup.
"""

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import WebItemsSequence
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class DeregisterProvider(PageObject):
    buttons = WebItemsSequence('button', cls=ButtonWithTextPageObject)

    def __str__(self):
        return 'Deregister provider popup'
