"""Utils and fixtures to facilitate spaces operations in op panel GUI."""

import re

from tests.gui.utils.common.common import Toggle, DropdownSelector
from tests.gui.utils.core.base import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import (Label, NamedButton, Button,
                                               Input, WebItem, WebItemsSequence,
                                               WebElement)
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class ImportConfiguration(PageObject):
    strategy_selector = WebItem('.ember-basic-dropdown', cls=DropdownSelector)
    max_depth = Input('.field-import_generic-maxDepth')


class UpdateConfiguration(PageObject):
    strategy_selector = WebItem('.ember-basic-dropdown', cls=DropdownSelector)
    max_depth = Input('.field-update_generic-maxDepth')
    scan_interval = Input('.field-update_generic-scanInterval')
    write_once = WebItem('.toggle-field-update_generic-writeOnce', cls=Toggle)
    delete_enabled = WebItem('.toggle-field-update_generic-deleteEnable',
                             cls=Toggle)


class SpaceSupportAddForm(PageObject):
    storage_selector = WebItem('.ember-basic-dropdown', cls=DropdownSelector)
    token = Input('input.field-main-token')
    size = Input('input.field-main-size')
    units = WebItemsSequence('.field-main-sizeUnit label.clickable',
                             cls=ButtonWithTextPageObject)
    mount_in_root = WebItem('.toggle-field-main-mountInRoot', cls=Toggle)
    import_storage_data = WebItem('.toggle-field-main-_importEnabled',
                                  cls=Toggle)
    import_configuration = WebItem('.import-configuration-section',
                                   cls=ImportConfiguration)
    update_configuration = WebItem('.update-configuration-section',
                                   cls=UpdateConfiguration)
    support_space = NamedButton('button', text='Support space')

    def __str__(self):
        return 'support space form in {}'.format(self.parent)


class SpaceSupportRecord(PageObject, ExpandableMixin):
    name = id = Label('.oneicon-space + .one-label')
    _toggle = WebElement('.one-collapsible-list-item-header')

    def is_expanded(self):
        return bool(re.match(r'.*\b(?<!-)opened\b.*',
                             self._toggle.get_attribute('class')))

    revoke_support = Button('button.btn-revoke-space')
    configure_data_import = Button('button.btn-import-storage-data')
    import_configuration = WebItem('.import-configuration-section',
                                   cls=ImportConfiguration)
    update_configuration = WebItem('.update-configuration-section',
                                   cls=UpdateConfiguration)
    save_configuration = NamedButton('button', text='Save configuration')

    Name = Label('td.item-table-content-cell .content-row:first-child '
                 '.one-label')
    Id = Label('td.item-table-content-cell .content-row:nth-child(2) '
               '.one-label')

    _import_strategy = WebElement('td.item-table-content-cell '
                                  '.content-row:nth-child(4)')
    _update_strategy = WebElement('td.item-table-content-cell '
                                  '.content-row:nth-child(5)')

    @property
    def import_strategy(self):
        default_values = {'Import strategy': 'Simple scan',
                          'Max depth': 'inf'}
        default_values.update(self._get_labels(self._import_strategy))
        return default_values

    @property
    def update_strategy(self):
        default_values = {'Update strategy': 'Simple scan',
                          'Max depth': 'inf',
                          'Scan interval [s]': '10',
                          'Write once': 'false',
                          'Delete enabled': 'false'}
        default_values.update(self._get_labels(self._update_strategy))
        return default_values

    @staticmethod
    def _get_labels(elem):
        items = elem.find_elements_by_css_selector('strong, .one-label')
        return {attr.text.strip(':'): val.text for attr, val
                in zip(items[::2], items[1::2])}


class SpacesContentPage(PageObject):
    spaces = WebItemsSequence('ul.one-collapsible-list li',
                              cls=SpaceSupportRecord)
    support_space = NamedButton('.btn-support-space', text='Support space')
    cancel_supporting_space = NamedButton('.btn-support-space',
                                          text='Cancel supporting space')
    form = WebItem('.support-space-form', cls=SpaceSupportAddForm)

    def __str__(self):
        return 'spaces page in {}'.format(self.parent)
