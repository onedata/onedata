"""Utils and fixtures to facilitate spaces operations in op panel GUI."""

import re

from tests.gui.utils.core.base import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import (Label, NamedButton, Button,
                                               Input, WebItem, WebItemsSequence,
                                               WebElement)
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject
from .storages import StorageSelector

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class SpaceSupportAddForm(PageObject):
    selected_storage = Label('.ember-power-select-selected-item')
    storage_selector = WebItem('.ember-basic-dropdown', cls=StorageSelector)
    token = Input('input.field-main-token')
    size = Input('input.field-main-size')
    units = WebItemsSequence('.field-main-sizeUnit label.clickable',
                             cls=ButtonWithTextPageObject)
    support_space = NamedButton('button', text='Support space')

    def __str__(self):
        return 'support space form in {}'.format(self.parent)


class SpaceSupportRecord(PageObject, ExpandableMixin):
    name = id = Label('.item-icon-container + .one-label')
    Name = Label('td.item-table-content-cell .content-row:first-child '
                 '.one-label')
    Id = Label('td.item-table-content-cell .content-row:nth-child(2) '
               '.one-label')
    revoke_support = Button('button.btn-revoke-space')
    _toggle = WebElement('.one-collapsible-list-item-header')

    def is_expanded(self):
        return bool(re.match(r'.*\b(?<!-)opened\b.*',
                             self._toggle.get_attribute('class')))


class SpacesContentPage(PageObject):
    spaces = WebItemsSequence('ul.one-collapsible-list li',
                              cls=SpaceSupportRecord)
    support_space = NamedButton('.btn-support-space', text='Support space')
    cancel_supporting_space = NamedButton('.btn-support-space',
                                          text='Cancel supporting space')
    form = WebItem('.support-space-form', cls=SpaceSupportAddForm)

    def __str__(self):
        return 'spaces page in {}'.format(self.parent)
