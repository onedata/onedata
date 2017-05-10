"""Utils and fixtures to facilitate storages control in panel GUI.
"""

from tests.gui.utils.common.common import Toggle
from tests.gui.utils.core.base import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import (WebItemsSequence, Label,
                                               NamedButton, Input,
                                               WebItem, WebElement)
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class StorageSelector(PageObject, ExpandableMixin):
    storages = WebItemsSequence('ul li', cls=ButtonWithTextPageObject)
    _toggle = WebElement('.ember-basic-dropdown-trigger[role="button"]')

    def __str__(self):
        return 'storage selector in {}'.format(self.parent)


class POSIX(PageObject):
    storage_name = Input('input.field-name')
    mount_point = Input('input.field-mountPoint')
    timeout = Input('input.field-timeout')
    read_only = WebItem('.toggle-field-readonly', cls=Toggle)

    def __str__(self):
        return 'POSIX {}'.format(self.parent)


class StorageAddForm(PageObject):
    selected_storage = Label('.ember-power-select-selected-item')
    storage_selector = WebItem('.ember-basic-dropdown', cls=StorageSelector)
    add = NamedButton('button', text='Add')
    posix = WebItem('form', cls=POSIX)

    def __str__(self):
        return 'add storage form in {}'.format(self.parent)


class StorageRecord(PageObject, ExpandableMixin):
    name = id = Label('.oneicon-provider + .one-label')
    storage_type = Label('td.item-table-content-cell .content-row:first-child '
                         '.one-label')
    mount_point = Label('td.item-table-content-cell .content-row:nth-child(2) '
                        '.one-label')
    _toggle = WebElement('.one-collapsible-list-item-header')

    def is_expanded(self):
        return True if 'opened' in self._toggle.get_attribute('class') else False


class StorageContentPage(PageObject):
    form = WebItem('.cluster-storage-add-form', cls=StorageAddForm)
    storages = WebItemsSequence('ul li.storage-item', cls=StorageRecord)
    add_storage = NamedButton('button', text='Add storage')
    cancel = NamedButton('button', text='Cancel')

    def __str__(self):
        return 'Storages content page in {}'.format(self.parent)
