"""Utils and fixtures to facilitate nodes operations in panel GUI.
"""

from tests.gui.utils.common.common import Toggle
from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import WebItemsSequence, Label, WebItem


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class HostRecord(PageObject):
    name = id = Label('td[data-option=name]',
                      parent_name='hosts table in cluster deployment step')
    database = WebItem('td[data-option=database]', cls=Toggle)
    cluster_worker = WebItem('td[data-option=clusterWorker]', cls=Toggle)
    cluster_manager = WebItem('td[data-option=clusterManager]', cls=Toggle)
    primary_cluster_manager = WebItem('td[data-th="Primary Cluster Manager"]',
                                      cls=Toggle)

    def __str__(self):
        return '{} record in {}'.format(self.name, self.parent)


class NodesContentPage(PageObject):
    hosts = WebItemsSequence('tr.cluster-host-table-row', cls=HostRecord)

    def __str__(self):
        return 'Nodes page in {}'.format(self.parent)
