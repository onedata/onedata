"""Utils and fixtures to facilitate operations on Onepanel oz gui clusters.
"""

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import WebItemsSequence, Label, NamedButton, Input, Toggle
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject

from tests.gui.utils.common.common import Sidebar, SidebarRecord

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class HostRecord(PageObject):
    name = id = Label('td[data-th="Hosts"]')
    database = Toggle('td[data-th="Database"]')
    cluster_worker = Toggle('td[data-th="Cluster Worker"]')
    cluster_manager = Toggle('td[data-th="Cluster Manager"]')
    primary_cluster_manager = Toggle('td[data-th="Primary Cluster Manager"]')

    def __str__(self):
        return 'host {} in {}'.format(self.name, self.parent)


class InstallationStep(PageObject):
    num = Label('ul.one-steps li.one-step.active .step-number')
    title = Label('ul.one-steps li.one-step.active .step-title')

    def __init__(self, *args, **kwargs):
        super(InstallationStep, self).__init__(*args, **kwargs)
        num = self.num
        cls = globals().get('Step{}'.format(num), None)
        if cls is not None:
            self.__class__ = cls
        else:
            raise RuntimeError('step {} not found in {}'.format(num,
                                                                self.parent))

    def __str__(self):
        return '{} in cluster deployment in {}'.format(self.title, self.parent)


class Step1(InstallationStep):
    nodes = WebItemsSequence('tr.cluster-host-table-row', cls=HostRecord)
    deploy = NamedButton('button', text='Deploy')


class Step2(InstallationStep):
    msg = Label('.row.text-center')
    manage_cluster = NamedButton('button', text='Manage the cluster')


class WelcomePage(PageObject):
    msg = Label('.row.text-center')
    create_new_cluster = NamedButton('button', text='Create new cluster')

    def __str__(self):
        return 'welcome page in {}'.format(self.parent)


class ClusterRecord(ButtonWithTextPageObject, SidebarRecord):
    @property
    def welcome_page(self):
        return WelcomePage(self.driver, self._content, self)

    @property
    def deployment(self):
        return InstallationStep(self.driver, self._content, self)


class Clusters(Sidebar):
    search_box = Input('ul.one-list li.search-bar-item')
    clusters = WebItemsSequence('ul.one-list li.one-list-item',
                                cls=ClusterRecord)
