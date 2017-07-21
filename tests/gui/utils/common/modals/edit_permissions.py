"""Utils and fixtures for operations on edit permissions modal.
"""

__author__ = "Michal Stanisz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.gui.utils.core.common import PageObject
from tests.gui.utils.common.modals.modal import Modal
from tests.gui.utils.core.web_elements import TextLabelWebElement,\
        WebItemsSequence, ButtonWebElement



class PermissionType(PageObject):
    name = id = TextLabelWebElement('.option-label')

    def __str__(self):
        return 'permission type option {} in {}'.format(self.name, self.parent)


class EditPermissionsModal(Modal):
    options = WebItemsSequence('.one-option-button-container',
                               cls=PermissionType)
    state = "posix"

    def get_input_box(self):
        if self.state == "posix":
            return self.web_elem.find_element_by_css_selector('.modal-row-main'
                    '.large-space input')
        raise RuntimeError("No input box in modal in {} state".format(self.state))

    def select(self, perm_type):
        self.state = perm_type.lower()
        for option in self.options:
            if option.name.lower() == perm_type.lower():
                option.web_elem.click()
    
    def __str__(self):
        return 'Edit permission modal'
