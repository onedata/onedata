"""Utils and fixtures to facilitate operations on revoke space support.
"""

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import NamedButton

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class RevokeSpaceSupport(PageObject):
    no = NamedButton('button', text='No, keep the support')
    yes = NamedButton('button', text='Yes, revoke')

    def __str__(self):
        return 'Revoke space support popup'
