"""Utils and fixtures for operations on edit permissions modal.
"""

__author__ = "Michal Stanisz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.gui.utils.core.common import PageObject
from tests.gui.utils.common.modals.modal import Modal
from tests.gui.utils.core.web_elements import (TextLabelWebElement,
        WebItemsSequence, InputWebElement, WebItem)


class PermissionType(PageObject):
    name = id = TextLabelWebElement('.option-label')

    def set(self):
        self.web_elem.click()

    def __str__(self):
        return 'permission type option {} in {}'.format(self.name, self.parent)


class POSIX(PageObject):
    value = InputWebElement('input')

    def __str__(self):
        return 'POSIX permission in {}'.format(self.parent)


class EditPermissionsModal(Modal):
    posix = WebItem('.modal-row-main.large-space', cls=POSIX)
    options = WebItemsSequence('.one-option-button-container',
                               cls=PermissionType)

    def select(self, perm_type):
        for option in self.options:
            if option.name.lower() == perm_type.lower():
                option.set()
    
    def __str__(self):
        return 'Edit permission modal'
