"""Utils and fixtures to facilitate deployment steps in panel GUI.
"""

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import (WebItemsSequence, Label, Button,
                                               NamedButton, Input, WebItem)
from .storages import StorageContentPage
from .nodes import HostRecord


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Step1(PageObject):
    """Used in both provider and zone panel"""
    hosts = WebItemsSequence('tr.cluster-host-table-row', cls=HostRecord)
    deploy = Button('button.btn-deploy-cluster')
    zone_name = Input('input.field-main-name')
    zone_domain_name = Input('input.field-main-domainName')

    def __str__(self):
        return str(self.parent)


class Step2(PageObject):
    """Used only in provider panel"""
    provider_name = Input('input.field-main-name')
    oz_domain = Input('input.field-main-onezoneDomainName')
    redirection_point = Input('input.field-main-redirectionPoint')
    latitude = Input('input.field-main-geoLatitude')
    longitude = Input('input.field-main-geoLongitude')
    register = NamedButton('button', text='Register')

    def __str__(self):
        return str(self.parent)


class Step3(StorageContentPage):
    """Used only in provider panel"""
    finish = NamedButton('button', text='Finish')

    def __str__(self):
        return str(self.parent)


class LastStep(PageObject):
    """Used in both provider and zone panel"""
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
