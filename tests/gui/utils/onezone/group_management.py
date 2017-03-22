"""Utils and fixtures to facilitate operations on GROUP MANAGEMENT panel
in Onezone web GUI.
"""

from tests.gui.utils.core.common import ExpandableMixin, PageObject
from tests.gui.utils.core.web_elements import TextLabelWebElement, WebItemsSequence, ButtonWithTextWebElement
from .common import OZPanel

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class _GroupRecord(PageObject, ExpandableMixin):
    name = id = TextLabelWebElement('.secondary-header',
                                    parent_name='given group record')

    def __str__(self):
        return 'group record named: "{}" in {}'.format(self.name, self.parent)


class GroupManagementPanel(OZPanel):
    groups = WebItemsSequence('.groups-list-list .secondary-accordion-group',
                              cls=_GroupRecord)
    _join_group_btn = ButtonWithTextWebElement('.clickable', text='join a group')

    def join_group(self):
        self._click_on_btn('join_group')
