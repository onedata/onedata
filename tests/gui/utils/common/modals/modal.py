"""Utils and fixtures to facilitate operations on modals.
"""

from abc import abstractmethod

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import Label

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Modal(PageObject):
    title = Label('.modal-title')

    @abstractmethod
    def __str__(self):
        pass
