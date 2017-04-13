"""Utils and fixtures to facilitate operations on Onepanel oz gui clusters.
"""

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import WebItemsSequence, Label, NamedButton, WebItem

from ..clusters import BasicClusters, BasicClusterRecord, BasicDeployment


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Step2(PageObject):
    msg = Label('.row.text-center')
    manage_cluster = NamedButton('button', text='Manage the cluster')

    def __str__(self):
        return str(self.parent)


class Deployment(BasicDeployment):
    step2 = WebItem('.steps-row + .row', cls=Step2)


class ClusterRecord(BasicClusterRecord):
    title = Label('.item-header', parent_name='clusters sidebar')

    @property
    def deployment(self):
        return Deployment(self.driver, self._content, self)


class Clusters(BasicClusters):
    clusters = WebItemsSequence('ul.one-list li.one-list-item',
                                cls=ClusterRecord)
