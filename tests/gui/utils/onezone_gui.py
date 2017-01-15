"""Utils and fixtures to facilitate operations on Onezone web GUI.
"""

import re

from tests.gui.utils.generic import find_web_elem
from tests.gui.utils.onezone.access_tokens import AccessTokensPanel
from tests.gui.utils.onezone.data_space_management import DataSpaceManagementPanel
from tests.gui.utils.onezone.manage_account import ManageAccount
from tests.gui.utils.onezone.providers import GoToYourFilesPanel
from tests.gui.utils.onezone.user_alias import UserAliasPanel
from tests.gui.utils.onezone.world_map import WorldMap

__author__ = "Jakub Liput, Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


RE_DATA_URL = re.compile(r'(?P<lang>/.*)?/data/(?P<space>.*)/(?P<dir>.*)')


class OZLoggedIn(object):
    panels = {'data space management': DataSpaceManagementPanel,
              'go to your files': GoToYourFilesPanel,
              'access tokens': AccessTokensPanel,
              'user alias': UserAliasPanel}

    def __init__(self, web_elem):
        self.web_elem = web_elem

    def __getitem__(self, panel):
        panel = panel.lower()
        cls = self.panels.get(panel, None)
        if cls:
            panel = panel.replace('_', ' ').lower()
            css_sel = '.main-accordion-group, a.main-accordion-toggle'
            items = self.web_elem.find_elements_by_css_selector(css_sel)
            for group, toggle in zip(items[::2], items[1::2]):
                if panel == toggle.text.lower():
                    return cls(group)

        elif panel == 'manage account':
            css_sel = 'header.onezone-top-bar'
            err_msg = 'no header for oz page found'
            header = find_web_elem(self.web_elem, css_sel, err_msg)
            return ManageAccount(header)

        elif panel == 'world map':
            css_sel = '.onezone-atlas'
            err_msg = 'no world map found on oz page'
            world_map = find_web_elem(self.web_elem, css_sel, err_msg)
            return WorldMap(world_map)
