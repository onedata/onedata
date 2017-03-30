"""Utils and fixtures to facilitate operations on modals.
"""

from abc import abstractmethod

from tests.gui.utils.common.common import PageObject
from tests.gui.utils.common.web_elements import TextLabelWebElement

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Modal(PageObject):
    title = TextLabelWebElement('.modal-title')

    @abstractmethod
    def __str__(self):
        pass
