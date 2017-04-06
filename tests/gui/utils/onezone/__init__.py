"""Utils and fixtures to facilitate operations on Onezone web GUI.
"""

from tests.gui.utils.core.web_elements import WebElement, WebElementsSequence
from .access_tokens import AccessTokensPanel
from .common import OZPanel
from .data_space_management import DataSpaceManagementPanel
from .go_to_your_files import GoToYourFilesPanel
from .group_management import GroupManagementPanel
from .manage_account import ManageAccount
from .user_alias import UserAliasPanel
from .world_map import WorldMap

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class OZLoggedIn(object):
    _atlas = WebElement('.onezone-atlas')
    _manage_account = WebElement('header.onezone-top-bar')
    _panels = WebElementsSequence('.main-accordion-group')

    panels = {'data space management': DataSpaceManagementPanel,
              'group management': GroupManagementPanel,
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
