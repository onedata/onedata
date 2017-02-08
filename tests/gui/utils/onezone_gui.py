"""Utils and fixtures to facilitate operations on Onezone web GUI.
"""

import re

from tests.gui.utils.onezone.manage_account import ManageAccount
from tests.gui.utils.onezone.panel import UserAliasPanel, AccessTokensPanel, \
    GoToYourFilesPanel, OZPanel, DataSpaceManagementPanel
from tests.gui.utils.onezone.world_map import WorldMap
from tests.gui.utils.common.web_elements import WebElement, ItemListWebElement, HeaderWebElement

__author__ = "Jakub Liput, Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


RE_DATA_URL = re.compile(r'(?P<lang>/.*)?/data/(?P<space>.*)/(?P<dir>.*)')


class OZLoggedIn(object):
    _atlas = WebElement('.onezone-atlas')
    _manage_account = HeaderWebElement('header.onezone-top-bar')
    _panels = ItemListWebElement('.main-accordion-group')

    panels = {'data space management': DataSpaceManagementPanel,
              'go to your files': GoToYourFilesPanel,
              'access tokens': AccessTokensPanel,
              'user alias': UserAliasPanel}

    def __init__(self, driver):
        self.web_elem = driver

    def __str__(self):
        return 'Onezone page'

    def __getitem__(self, item):
        item = item.lower()
        cls = self.panels.get(item, None)
        if cls:
            item = item.replace('_', ' ').lower()
            for panel in (OZPanel(self.web_elem, panel, self)
                          for panel in self._panels):
                if item == panel.name.lower():
                    panel.__class__ = cls
                    return panel

        elif item == 'manage account':
            return ManageAccount(self.web_elem, self._manage_account, self)
        elif item == 'world map':
            return WorldMap(self.web_elem, self._atlas, self)
        else:
            raise RuntimeError('no "{}" on {} found'.format(item, str(self)))
