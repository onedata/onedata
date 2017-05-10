"""Utils and fixtures to facilitate deployment steps in panel GUI.
"""

from tests.gui.utils.common.common import Toggle
from tests.gui.utils.core.base import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import (WebItemsSequence, Label,
                                               Button, NamedButton, Input,
                                               WebItem, WebElement)
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject

from .clusters import HostRecord

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Step1(PageObject):
    """Used in both provider and zone panel"""
    hosts = WebItemsSequence('tr.cluster-host-table-row', cls=HostRecord)
    deploy = Button('button.btn-deploy-cluster')
    zone_name = Input('input.field-name')

    def __str__(self):
        return str(self.parent)


class Step2(PageObject):
    """Used only in provider panel"""
    msg = Label('.content-row')
    provider_name = Input('input.field-name')
    oz_domain = Input('input.field-onezoneDomainName')
    redirection_point = Input('input.field-redirectionPoint')
    latitude = Input('input.field-geoLatitude')
    longitude = Input('input.field-geoLongitude')
    register = NamedButton('button', text='Register')

    def __str__(self):
        return str(self.parent)


class StorageTypeSelector(PageObject, ExpandableMixin):
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
    storage_selector = WebItem('.ember-basic-dropdown', cls=StorageTypeSelector)
    add = NamedButton('button', text='Add')
    posix = WebItem('form', POSIX)

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


class Step3(PageObject):
    """Used only in provider panel"""
    form = WebItem('.cluster-storage-add-form', cls=StorageAddForm)
    storages = WebItemsSequence('ul li.storage-item', cls=StorageRecord)
    add_storage = NamedButton('button', text='Add storage')
    cancel = NamedButton('button', text='Cancel')
    finish = NamedButton('button', text='Finish')

    def __str__(self):
        return str(self.parent)


class LastStep(PageObject):
    """Used in both provider and zone panel"""
    msg = Label('.row.text-center')
    manage_the_cluster = NamedButton('button', text='Manage the cluster')

    def __str__(self):
        return str(self.parent)


class Deployment(PageObject):
    num = Label('ul.one-steps li.one-step.active .step-number')
    title = Label('ul.one-steps li.one-step.active .step-title',
                  parent_name='cluster deployment step')

    _deployment_step_css = '.steps-row + .row'
    step1 = WebItem(_deployment_step_css, cls=Step1)
    step2 = WebItem(_deployment_step_css, cls=Step2)
    step3 = WebItem(_deployment_step_css, cls=Step3)
    laststep = WebItem(_deployment_step_css, cls=LastStep)

    def __str__(self):
        return '{} deployment step in {}'.format(self.title, self.parent)
