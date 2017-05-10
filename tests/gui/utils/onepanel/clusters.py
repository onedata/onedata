"""Utils and fixtures to facilitate common operations on clusters sidebar.
"""

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import (WebItemsSequence, Label,
                                               NamedButton, Input)
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class WelcomePage(PageObject):
    msg = Label('.row.text-center')
    create_new_cluster = NamedButton('button', text='Create new cluster')

    def __str__(self):
        return 'welcome page in {}'.format(self.parent)


class ClusterRecord(ButtonWithTextPageObject):
    name = id = Label('.item-header', parent_name='clusters sidebar')
    submenu = WebItemsSequence('ul.one-list-level-2 li',
                               cls=ButtonWithTextPageObject)

    def __str__(self):
        return '{} item in {}'.format(self.name, self.parent)


class Clusters(PageObject):
    search_box = Input('ul.one-list li.search-bar-item input')
    items = WebItemsSequence('ul.one-list li.one-list-item',
                             cls=ClusterRecord)
