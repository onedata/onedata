"""Utils to facilitate storages control in panel GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


import re

from tests.gui.utils.common.common import Toggle, DropdownSelector
from tests.gui.utils.core.base import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import (WebItemsSequence, Label,
                                               NamedButton, Input,
                                               WebItem, WebElement)


class POSIX(PageObject):
    storage_name = Input('input.field-generic-name')
    mount_point = Input('input.field-posix-mountPoint')
    timeout = Input('input.field-posix-timeout')
    read_only = Toggle('.toggle-field-posix-readonly')


class StorageAddForm(PageObject):
    storage_selector = DropdownSelector('.ember-basic-dropdown')
    add = NamedButton('button', text='Add')
    posix = WebItem('form', cls=POSIX)


class StorageRecord(PageObject, ExpandableMixin):
    name = id = Label('.item-icon-container + .one-label')
    storage_type = Label('td.item-table-content-cell .content-row:first-child '
                         '.one-label')
    mount_point = Label('td.item-table-content-cell .content-row:nth-child(3) '
                        '.one-label')
    _toggle = WebElement('.one-collapsible-list-item-header')

    def is_expanded(self):
        return bool(re.match(r'.*\b(?<!-)opened\b.*',
                             self._toggle.get_attribute('class')))


class StorageContentPage(PageObject):
    form = WebItem('.cluster-storage-add-form', cls=StorageAddForm)
    storages = WebItemsSequence('ul li .storage-item', cls=StorageRecord)
    add_storage = NamedButton('button', text='Add storage')
    cancel = NamedButton('button', text='Cancel')
