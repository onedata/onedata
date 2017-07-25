"""Utils to facilitate spaces operations in op panel GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


import re

from tests.gui.utils.common.common import Toggle, DropdownSelector
from tests.gui.utils.core.base import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import (Label, NamedButton, Button,
                                               Input, WebItem, WebElement,
                                               WebItemsSequence)
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject


DEFAULT_IMPORT_STRATEGY_CONFIG = {'Import strategy': 'Simple scan',
                                  'Max depth': '65535'}

DEFAULT_UPDATE_STRATEGY_CONFIG = {'Update strategy': 'Simple scan',
                                  'Max depth': '65535',
                                  'Scan interval [s]': '10',
                                  'Write once': 'false',
                                  'Delete enabled': 'false'}


class ImportConfigurationForm(PageObject):
    strategy_selector = DropdownSelector('.ember-basic-dropdown')
    max_depth = Input('.field-import_generic-maxDepth')


class UpdateConfigurationForm(PageObject):
    strategy_selector = DropdownSelector('.ember-basic-dropdown')
    max_depth = Input('.field-update_generic-maxDepth')
    scan_interval = Input('.field-update_generic-scanInterval')
    write_once = Toggle('.toggle-field-update_generic-writeOnce')
    delete_enabled = Toggle('.toggle-field-update_generic-deleteEnable')


class SpaceSupportAddForm(PageObject):
    storage_selector = DropdownSelector('.ember-basic-dropdown')
    token = Input('input.field-main-token')
    size = Input('input.field-main-size')
    units = WebItemsSequence('.field-main-sizeUnit label.clickable',
                             cls=ButtonWithTextPageObject)
    mount_in_root = Toggle('.toggle-field-main-mountInRoot')
    import_storage_data = Toggle('.toggle-field-main-_importEnabled')

    import_configuration = WebItem('.import-configuration-section',
                                   cls=ImportConfigurationForm)
    update_configuration = WebItem('.update-configuration-section',
                                   cls=UpdateConfigurationForm)

    support_space = NamedButton('button', text='Support space')


class SpaceInfo(PageObject):
    space_name = Label('.space-name')
    space_id = Label('.space-id')
    storage_name = Label('.space--provider-storage')
    _import_strategy = WebElement('.space-import')
    _update_strategy = WebElement('.space-update')
    _mount_in_root = WebElement('.space-mountInRoot')

    def is_mount_in_root_enabled(self):
        return 'disabled' not in self._mount_in_root.get_attribute('class')

    @property
    def import_strategy(self):
        values = DEFAULT_IMPORT_STRATEGY_CONFIG.copy()
        values.update(self._get_labels(self._import_strategy))
        return values

    @property
    def update_strategy(self):
        values = DEFAULT_UPDATE_STRATEGY_CONFIG.copy()
        values.update(self._get_labels(self._update_strategy))
        return values

    @staticmethod
    def _get_labels(elem):
        items = elem.find_elements_by_css_selector('strong, .one-label')
        return {attr.text.strip(':'): val.text for attr, val
                in zip(items[::2], items[1::2])}


class SpaceRecord(PageObject, ExpandableMixin):
    name = id = Label('.item-icon-container + .one-label')
    toolbar = Button('.collapsible-toolbar-toggle')
    info = WebItem('.space-info', cls=SpaceInfo)

    _toggle = WebElement('.one-collapsible-list-item-header')

    def is_expanded(self):
        return bool(re.match(r'.*\b(?<!-)opened\b.*',
                             self._toggle.get_attribute('class')))

    import_configuration = WebItem('.import-configuration-section',
                                   cls=ImportConfigurationForm)
    update_configuration = WebItem('.update-configuration-section',
                                   cls=UpdateConfigurationForm)
    save_configuration = NamedButton('button', text='Save configuration')


class SpacesContentPage(PageObject):
    spaces = WebItemsSequence('ul.one-collapsible-list '
                              '.cluster-spaces-table-item', cls=SpaceRecord)
    support_space = NamedButton('.btn-support-space', text='Support space')
    form = WebItem('.support-space-form', cls=SpaceSupportAddForm)
    cancel_supporting_space = NamedButton('.btn-support-space',
                                          text='Cancel supporting space')
