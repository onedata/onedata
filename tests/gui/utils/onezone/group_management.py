"""Utils and fixtures to facilitate operations on GROUP MANAGEMENT panel
in Onezone web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.utils.core.base import ExpandableMixin, PageObject
from tests.gui.utils.core.web_elements import Label, WebItemsSequence, NamedButton
from .common import OZPanel


class GroupRecord(PageObject, ExpandableMixin):
    name = id = Label('.secondary-header', parent_name='given group record')

    def __str__(self):
        return 'group record named: "{}" in {}'.format(self.name, self.parent)


class GroupManagementPanel(OZPanel):
    groups = WebItemsSequence('.groups-list-list .secondary-accordion-group',
                              cls=GroupRecord)
    join_group = NamedButton('.clickable', text='join a group')
