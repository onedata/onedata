"""Utils and fixtures to facilitate operations on Onepanel of zone web GUI.
"""

from tests.gui.utils.core.web_elements import WebItem

from tests.gui.utils.common.common import OnePage
from .clusters import Clusters

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Sidebar:
    pass


class Content:
    pass


class Onepanel(OnePage):
    sidebar = WebItem('#col-sidebar', cls=Sidebar)
    content = WebItem('.col-content', cls=Content)


class OZPanel(Onepanel):
    def __str__(self):
        return 'Onepanel zone page'


class OPPanel(Onepanel):
    def __str__(self):
        return 'Onepanel provider page'
