"""Utils and fixtures to facilitate spaces operations in op panel GUI."""

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import (Label, Button, NamedButton,
                                               Input, WebItem)
from .storages import StorageSelector

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class SpaceSupportAddForm(PageObject):
    selected_storage = Label('.ember-power-select-selected-item')
    storage_selector = WebItem('.ember-basic-dropdown', cls=StorageSelector)
    token = Input('input.field-token')
    size = Input('input.field-size')
    mb = Button('input.field-sizeUnit-mb')
    gb = Button('input.field-sizeUnit-gb')
    tb = Button('input.field-sizeUnit-tb')
    support_space = NamedButton('button', text='Support space')

    def __str__(self):
        return 'support space form in {}'.format(self.parent)


class SpacesContentPage(PageObject):
    support_space = NamedButton('.btn-support-space', text='Support space')
    cancel_supporting_space = NamedButton('.btn-support-space',
                                          text='Cancel supporting space')
    form = WebItem('.support-space-form', cls=SpaceSupportAddForm)

    def __str__(self):
        return 'spaces page in {}'.format(self.parent)
