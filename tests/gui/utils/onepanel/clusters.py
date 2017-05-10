"""Utils and fixtures to facilitate common operations on clusters sidebar.
"""

from tests.gui.utils.common.common import Sidebar, SidebarRecord, Toggle
from tests.gui.utils.core.base import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import (WebItemsSequence, Label, Button,
                                               NamedButton, Input, WebItem,
                                               WebElement)
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject
from .deployment import Deployment

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class WelcomePage(PageObject):
    msg = Label('.row.text-center')
    create_new_cluster = NamedButton('button', text='Create new cluster')

    def __str__(self):
        return 'welcome page in {}'.format(self.parent)


class StorageSelector(PageObject, ExpandableMixin):
    storages = WebItemsSequence('ul li', cls=ButtonWithTextPageObject)
    _toggle = WebElement('.ember-basic-dropdown-trigger[role="button"]')

    def __str__(self):
        return 'storage selector in {}'.format(self.parent)


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


class Spaces(PageObject):
    support_space = NamedButton('.btn-support-space', text='Support space')
    cancel_supporting_space = NamedButton('.btn-support-space',
                                          text='Cancel supporting space')
    add_support_form = WebItem('.support-space-form', cls=SpaceSupportAddForm)

    def __str__(self):
        return 'spaces page in {}'.format(self.parent)


class ClusterRecord(ButtonWithTextPageObject, SidebarRecord):
    title = id = Label('.item-header', parent_name='clusters sidebar')
    submenu = WebItemsSequence('ul.one-list-level-2 li',
                               cls=ButtonWithTextPageObject)

    @property
    def welcome_page(self):
        return WelcomePage(self.driver, self._content, self)

    @property
    def deployment(self):
        return Deployment(self.driver, self._content, self)

    @property
    def spaces(self):
        return Spaces(self.driver, self._content, self)

    @property
    def nodes(self):
        return Nodes(self.driver, self._content, self)

    def __str__(self):
        return '{} record in {}'.format(self.title, self.parent)


class Clusters(Sidebar):
    search_box = Input('ul.one-list li.search-bar-item input')
    records = WebItemsSequence('ul.one-list li.one-list-item',
                               cls=ClusterRecord)
