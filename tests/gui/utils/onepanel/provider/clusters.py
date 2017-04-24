"""Utils and fixtures to facilitate operations on Onepanel oz gui clusters.
"""

from tests.gui.utils.core.base import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import WebItemsSequence, Label, NamedButton, WebItem, Input, WebElement
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject
from tests.gui.utils.common.common import Toggle

from ..clusters import BasicClusters, BasicClusterRecord, BasicDeployment


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Step2(PageObject):
    msg = Label('.content-row')
    provider_name = Input('input.one-form-field-name')
    oz_domain = Input('input.one-form-field-onezoneDomainName')
    redirection_point = Input('input.one-form-field-redirectionPoint')
    latitude = Input('input.one-form-field-geoLatitude')
    longitude = Input('input.one-form-field-geoLongitude')
    register = NamedButton('button', text='Register')

    def __str__(self):
        return str(self.parent)


class StorageSelector(PageObject, ExpandableMixin):
    storages = WebItemsSequence('ul li', cls=ButtonWithTextPageObject)
    _toggle = WebElement('.ember-basic-dropdown-trigger[role="button"]')

    def __str__(self):
        return 'storage selector in {}'.format(self.parent)


class POSIX(PageObject):
    storage_name = Input('input.one-form-field-name')
    mount_point = Input('input.one-form-field-mountPoint')
    timeout = Input('input.one-form-field-timeout')
    read_only = WebItem('.toggle-field-readonly', cls=Toggle)

    def __str__(self):
        return str(self.parent)


class StorageAddForm(PageObject):
    storage_selector = WebItem('.ember-basic-dropdown', cls=StorageSelector)
    selected_storage = Label('.ember-power-select-selected-item')
    _storage_add_form = WebElement('.cluster-storage-add-form')

    @property
    def form(self):
        curr_storage = self.selected_storage
        cls = globals().get(curr_storage, None)
        if cls is not None:
            return cls(self.driver, self._storage_add_form, self)
        else:
            raise RuntimeError('no {} storage found'.format(curr_storage))

    add = NamedButton('button', text='Add')

    def __str__(self):
        return 'add storage form in {}'.format(self.parent)


class StorageRecord(PageObject):
    pass


class Step3(PageObject):
    storage_add_form = WebItem('.cluster-storage-add-form', cls=StorageAddForm)
    storages = WebItemsSequence('ul li.one-collapsible-list-item',
                                cls=StorageRecord)

    add_storage = NamedButton('button', text='Add storage')
    cancel = NamedButton('button', text='Cancel')
    finish = NamedButton('button', text='Finish')

    def __str__(self):
        return str(self.parent)


class Deployment(BasicDeployment):
    step2 = WebItem('.steps-row + .row', cls=Step2)
    step3 = WebItem('.steps-row + .row', cls=Step3)


class ClusterRecord(BasicClusterRecord):
    @property
    def deployment(self):
        return Deployment(self.driver, self._content, self)


class Clusters(BasicClusters):
    clusters = WebItemsSequence('ul.one-list li.one-list-item',
                                cls=ClusterRecord)
