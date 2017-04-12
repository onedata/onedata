"""Utils and fixtures to facilitate operations on File distribution modal.
"""

from tests.gui.utils.core.web_elements import Label
from .modal import Modal

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class DeployingClusterModal(Modal):
    msg = Label('.new-cluster-deploy-progress h1')
    progress = Label('.progress-bar')

    def __str__(self):
        return 'Deploying cluster modal'
