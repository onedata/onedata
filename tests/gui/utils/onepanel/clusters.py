"""Utils and fixtures to facilitate common operations on clusters sidebar.
"""

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import WebItemsSequence, Label, NamedButton, Input, WebItem
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject

from tests.gui.utils.common.common import Sidebar, SidebarRecord, Toggle

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class HostRecord(PageObject):
    name = id = Label('td[data-th="Hosts"]',
                      parent_name='hosts table in cluster deployment step')
    database = WebItem('td[data-th="Database"]', cls=Toggle)
    cluster_worker = WebItem('td[data-th="Cluster Worker"]', cls=Toggle)
    cluster_manager = WebItem('td[data-th="Cluster Manager"]', cls=Toggle)
    primary_cluster_manager = WebItem('td[data-th="Primary Cluster Manager"]',
                                      cls=Toggle)

    def __str__(self):
        return '{} record in {}'.format(self.name, self.parent)


class Step1(PageObject):
    hosts = WebItemsSequence('tr.cluster-host-table-row', cls=HostRecord)
    deploy = NamedButton('button', text='Deploy')

    def __str__(self):
        return str(self.parent)


class WelcomePage(PageObject):
    msg = Label('.row.text-center')
    create_new_cluster = NamedButton('button', text='Create new cluster')

    def __str__(self):
        return 'welcome page in {}'.format(self.parent)


class BasicDeployment(PageObject):
    num = Label('ul.one-steps li.one-step.active .step-number')
    title = Label('ul.one-steps li.one-step.active .step-title',
                  parent_name='cluster deployment step')
    step1 = WebItem('.steps-row + .row', cls=Step1)

    def __str__(self):
        return '{} deployment step in {}'.format(self.title, self.parent)


class BasicClusterRecord(ButtonWithTextPageObject, SidebarRecord):
    title = id = Label('.item-header', parent_name='clusters sidebar')

    @property
    def welcome_page(self):
        return WelcomePage(self.driver, self._content, self)

    def __str__(self):
        return '{} record in {}'.format(self.title, self.parent)


class BasicClusters(Sidebar):
    search_box = Input('ul.one-list li.search-bar-item input')
