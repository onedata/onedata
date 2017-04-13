"""Utils and fixtures to facilitate operations on Onepanel of provider web GUI.
"""

from tests.gui.utils.common.common import OnePage
from .clusters import Clusters

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class OPPanel(OnePage):
    _sidebars = {'CLUSTERS': Clusters}

    def __str__(self):
        return 'Onepanel of provider page'
