"""Utils and fixtures to facilitate operation on groups in Oneprovider GUI"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.utils.core.base import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import (Label, WebItemsSequence,
                                               WebItem, Button, WebElement)
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject


class SettingDropdown(PageObject, ExpandableMixin):
    options = WebItemsSequence('ul.dropdown-menu-list li.clickable',
                               cls=ButtonWithTextPageObject)
    _toggle = WebElement('.dropdown-toggle[data-toggle="dropdown"]')


class GroupSidebarRecord(PageObject):
    name = id = Label('.item-icon + .item-label')
    settings = WebItem('.settings-dropdown', cls=SettingDropdown)
    members = Button('ul li.members-permissions .item-click-area')

    def is_selected(self):
        return 'active' in self.web_elem.get_attribute('class')


class GroupsSidebar(PageObject):
    title = Label('.secondary-sidebar-header .title')
    create = Button('.oneicon-group-add')
    join = Button('.oneicon-join')
    groups = WebItemsSequence('ul.groups-list li.first-level',
                              cls=GroupSidebarRecord)


class PermissionTableRow(PageObject):
    name = id = Label('.one-first .truncated-string-content')


class PermissionTable(PageObject):
    users = WebItemsSequence('table.permissions tbody:first-of-type',
                             cls=PermissionTableRow)
    groups = WebItemsSequence('table.permissions tbody:nth-of-type(2)',
                              cls=PermissionTableRow)
    save = Button('button.btn-save')
    discard = Button('button.btn-discard')


class GroupContentPage(PageObject):
    sidebar = WebItem('.secondary-sidebar', cls=GroupsSidebar)
    permission_table = WebItem('#content-scroll .permissions-table',
                               cls=PermissionTable)
