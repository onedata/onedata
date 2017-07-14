"""Utils and fixtures to facilitate operations on File distribution modal.
"""

from tests.gui.utils.core.web_elements import NamedButton
from .modal import Modal

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class RevokeSpaceSupportModal(Modal):
    no = NamedButton('button', text='No, keep the support')
    yes = NamedButton('button', text='Yes, revoke')

    def __str__(self):
        return 'Deploying cluster modal'
